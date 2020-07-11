{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
-- |

module Test.Sandwich.Interpreters.RunTree (
  runTreeMain
  , RunTreeContext(..)
  , getImmediateChildren
  ) where

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Monad
import Control.Monad.Free
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import qualified Data.List as L
import Data.Maybe
import Data.Sequence as Seq hiding ((:>))
import Data.String.Interpolate
import Data.Time.Clock
import System.Directory
import System.FilePath
import System.IO
import Test.Sandwich.Interpreters.RunTree.Logging
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.Types.Options
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Test.Sandwich.Util


runTreeMain :: BaseContext -> Free (SpecCommand BaseContext IO) () -> IO [RunTree]
runTreeMain baseContext spec = do
  asyncBaseContext <- async $ return baseContext
  runReaderT (runTreeSequentially spec) $ RunTreeContext {
    runTreeContext = asyncBaseContext
    , runTreeOptions = baseContextOptions baseContext
    , runTreeIndexInParent = 0
    , runTreeNumSiblings = countChildren spec
    , runTreeCurrentFolder = baseContextRunRoot baseContext
    }


runTree :: (HasBaseContext context) => Free (SpecCommand context IO) r -> ReaderT (RunTreeContext context) IO [RunTree]
runTree (Free (Before l f subspec next)) = do
  (status, logs, toggled, rtc@RunTreeContext {..}) <- getInfo

  newContextAsync <- liftIO $ async $ do
    (addPathSegment (PathSegment l True runTreeIndexInParent runTreeNumSiblings) -> ctx) <- wait runTreeContext
    getCurrentTime >>= atomically . writeTVar status . Running
    (runExampleM f ctx logs (Just [i|Exception in before '#{l}' handler|])) >>= \case
      Failure e  -> throwIO e
      Success -> do
        endTime <- getCurrentTime
        atomically $ modifyTVar status $ \case
          Running startTime -> Done startTime endTime Success
          _ -> Done endTime endTime Success
        return ctx

  subtree <- local (const $ rtc { runTreeContext = newContextAsync
                                , runTreeCurrentFolder = appendFolder rtc l }) $ runTreeSequentially subspec

  myAsync <- liftIO $ liftIO $ asyncWithUnmask $ \unmask -> do
    flip withException (recordExceptionInStatus status) $ unmask $ do
      -- Does not throw exception
      void $ waitForTree subtree
      -- Throws exception if the newContextAsync does
      -- Placed after the waitForTree to ensure we wait for the subtree on exception
      void $ wait newContextAsync

  continueWith (RunTreeGroup l toggled status (appendFolder rtc l) True subtree logs myAsync) next
runTree (Free (After l f subspec next)) = do
  (status, logs, toggled, rtc@RunTreeContext {..}) <- getInfo

  newContextAsync <- liftIO $ async $ do
    ctx <- wait runTreeContext
    return (addPathSegment (PathSegment l True runTreeIndexInParent runTreeNumSiblings) ctx)

  subtree <- local (const $ rtc { runTreeContext = newContextAsync
                                , runTreeCurrentFolder = appendFolder rtc l }) $ runTreeSequentially subspec

  myAsync <- liftIO $ async $ do
    _ <- waitForTree subtree
    (addPathSegment (PathSegment l True runTreeIndexInParent runTreeNumSiblings) -> ctx) <- wait runTreeContext
    startTime <- getCurrentTime
    atomically $ writeTVar status (Running startTime)
    result <- runExampleM f ctx logs (Just [i|Exception in after '#{l}' handler|])
    endTime <- getCurrentTime
    atomically $ writeTVar status $ Done startTime endTime result

  continueWith (RunTreeGroup l toggled status (appendFolder rtc l) True subtree logs myAsync) next
runTree (Free (Around l f subspec next)) = do
  (status, logs, toggled, rtc@RunTreeContext {..}) <- getInfo

  -- Use mvar to control when subspec is allowed to run
  mvar :: MVar () <- liftIO newEmptyMVar
  newContextAsync <- liftIO $ async $ do
    (addPathSegment (PathSegment l False runTreeIndexInParent runTreeNumSiblings) -> ctx) <- waitAndWrapContextException runTreeContext
    void $ readMVar mvar
    return ctx

  subtree <- withReaderT (const $ rtc { runTreeContext = newContextAsync
                                      , runTreeCurrentFolder = appendFolder rtc l }) $ runTreeSequentially subspec

  myAsync <- liftIO $ async $ do
    (addPathSegment (PathSegment l True runTreeIndexInParent runTreeNumSiblings) -> ctx) <- waitAndWrapContextException runTreeContext
    startTime <- getCurrentTime
    atomically $ writeTVar status (Running startTime)
    let action = putMVar mvar () >> void (waitForTree subtree)
    result <- runExampleM (f action) ctx logs (Just [i|Exception in around '#{l}' handler|])
    endTime <- getCurrentTime
    atomically $ writeTVar status $ Done startTime endTime result

  continueWith (RunTreeGroup l toggled status (appendFolder rtc l) True subtree logs myAsync) next
runTree (Free (Introduce l _cl alloc cleanup subspec next)) = do
  (status, logs, toggled, rtc@RunTreeContext {..}) <- getInfo

  mvar <- liftIO newEmptyMVar
  newContextAsync <- liftIO $ async $ readMVar mvar

  subtree <- withReaderT (const $ rtc { runTreeContext = newContextAsync
                                      , runTreeCurrentFolder = appendFolder rtc l }) $ runTreeSequentially subspec

  myAsync <- liftIO $ asyncWithUnmask $ \unmask -> do
    -- On any async exception, record it in our status and throw it to the context async
    -- (to ensure no child ends up waiting on it forever. the child may have already finished waiting on it at this point)
    flip withException (\e -> recordExceptionInStatus status e >> cancelWith newContextAsync e) $ unmask $ do
      bracket (do
                  (addPathSegment (PathSegment l True runTreeIndexInParent runTreeNumSiblings) -> ctx) <- waitAndWrapContextException runTreeContext
                  startTime <- getCurrentTime
                  atomically $ writeTVar status (Running startTime)
                  (runExampleM' alloc ctx logs (Just [i|Exception in allocation '#{l}' handler|])) >>= \case
                    Right x -> do
                      let newCtx = LabelValue x :> ctx -- TODO: modify base context here to prepend path segment?
                      putMVar mvar newCtx
                      return $ Right newCtx
                    Left err -> do
                      cancelWith newContextAsync err
                      return $ Left err
              )
              (\eitherStatus -> do
                  finalStatus <- case eitherStatus of
                    Right (LabelValue intro :> ctx) -> runExampleM (cleanup intro) ctx logs (Just [i|Exception in cleanup '#{l}' handler|])
                    Left e -> return $ Failure e

                  endTime <- getCurrentTime
                  atomically $ modifyTVar status $ \case
                    Running startTime -> Done startTime endTime finalStatus
                    _ -> Done endTime endTime finalStatus
              )
              (\_ -> do
                  void $ waitForTree subtree
              )
  continueWith (RunTreeGroup l toggled status (appendFolder rtc l) True subtree logs myAsync) next
runTree (Free (IntroduceWith l _cl action subspec next)) = do
  (status, logs, toggled, rtc@RunTreeContext {..}) <- getInfo

  mvar <- liftIO newEmptyMVar
  newContextAsync <- liftIO $ async $ readMVar mvar
  subtree <- withReaderT (const $ rtc { runTreeContext = newContextAsync
                                      , runTreeCurrentFolder = appendFolder rtc l }) $ runTreeSequentially subspec

  myAsync <- liftIO $ asyncWithUnmask $ \unmask ->
    flip withException (\e -> recordExceptionInStatus status e >> cancelWith newContextAsync e) $ unmask $ do
      (addPathSegment (PathSegment l False runTreeIndexInParent runTreeNumSiblings) -> ctx) <- waitAndWrapContextException runTreeContext
      startTime <- liftIO getCurrentTime

      let wrappedAction = do
            liftIO $ atomically $ writeTVar status (Running startTime)

            let cleanup = liftIO $ do
                  contents <- tryReadMVar mvar
                  when (isNothing contents) $
                    -- The action failed to produce a value; cancel newContextAsync with an exception
                    cancelWith newContextAsync $ Reason Nothing [i|Action in introduceWith '#{l}' failed to produce a value|]

            eitherResult <- tryAny $ flip finally cleanup $
              action $ \x -> do
                liftIO $ putMVar mvar (LabelValue x :> ctx)
                void $ waitForTree subtree

            endTime <- liftIO getCurrentTime
            liftIO $ atomically $ writeTVar status $ Done startTime endTime $ case eitherResult of
              Left e -> Failure (GotException Nothing (Just "Unknown exception") (SomeExceptionWithEq e))
              Right _ -> Success

      runExampleM wrappedAction ctx logs (Just [i|Exception in introduceWith '#{l}'|]) >>= \case
        Success -> return ()
        Failure e -> do
          endTime <- liftIO getCurrentTime
          atomically $ writeTVar status $ Done startTime endTime (Failure e)
          cancelWith newContextAsync e

  continueWith (RunTreeGroup l toggled status (appendFolder rtc l) True subtree logs myAsync) next
runTree (Free (It l ex next)) = do
  (status, logs, toggled, rtc@RunTreeContext {..}) <- getInfo

  myAsync <- liftIO $ asyncWithUnmask $ \unmask -> do
    flip withException (recordExceptionInStatus status) $ unmask $ do
      (tryAny $ wait runTreeContext) >>= \case
        Left (e :: SomeException) -> do
          endTime <- getCurrentTime
          atomically $ writeTVar status $ Done endTime endTime $ Failure (GetContextException Nothing (SomeExceptionWithEq e))
        Right (addPathSegment (PathSegment l False runTreeIndexInParent runTreeNumSiblings) -> ctx) -> do
          startTime <- getCurrentTime
          atomically $ writeTVar status (Running startTime)
          result <- runExampleM ex ctx logs Nothing
          endTime <- getCurrentTime
          atomically $ writeTVar status $ Done startTime endTime result
  continueWith (RunTreeSingle l toggled status (appendFolder rtc l) logs myAsync) next
runTree (Free (Describe l subspec next)) = do
  (_, _, _, rtc@RunTreeContext {..}) <- getInfo

  newContextAsync <- liftIO $ async $ do
    ctx <- wait runTreeContext
    return $ modifyBaseContext ctx (\x@(BaseContext {..}) -> x {
                                       baseContextPath = baseContextPath
                                         |> PathSegment l False runTreeIndexInParent runTreeNumSiblings
                                       })

  subtree <- local (const $ rtc { runTreeContext = newContextAsync
                                , runTreeIndexInParent = 0
                                , runTreeNumSiblings = countChildren subspec
                                , runTreeCurrentFolder = appendFolder rtc l }) $ runTreeSequentially subspec
  runDescribe False l subtree next
runTree (Free (Parallel subspec next)) = do
  (_, _, _, rtc@RunTreeContext {..}) <- getInfo

  newContextAsync <- liftIO $ async $ do
    ctx <- wait runTreeContext
    return $ modifyBaseContext ctx (\x@(BaseContext {..}) -> x {
                                       baseContextPath = baseContextPath
                                         |> PathSegment "parallel" False runTreeIndexInParent runTreeNumSiblings
                                       })

  subtree <- local (const $ rtc { runTreeContext = newContextAsync
                                , runTreeIndexInParent = 0
                                , runTreeNumSiblings = countChildren subspec
                                , runTreeCurrentFolder = appendFolder rtc "parallel"}) $ runTree subspec
  runDescribe True "parallel" subtree next
runTree (Pure _) = return []


runDescribe :: (HasBaseContext context) => Bool -> String -> [RunTree] -> SpecFree context IO r -> ReaderT (RunTreeContext context) IO [RunTree]
runDescribe isContextManager l subtree next = do
  (status, logs, toggled, rtc@RunTreeContext {..}) <- getInfo

  tree <- RunTreeGroup l toggled status (appendFolder rtc l) isContextManager subtree logs <$> do
    liftIO $ asyncWithUnmask $ \unmask -> do
      flip withException (recordExceptionInStatus status) $ unmask $ do
        void $ waitCatch runTreeContext -- Wait for context to be ready, but don't throw
        startTime <- getCurrentTime
        atomically $ writeTVar status (Running startTime)
        treeResult <- waitForTree subtree
        void $ waitAndWrapContextException runTreeContext -- Throw if the context wasn't ready, now that we're sure we waited for the subtree.
        endTime <- getCurrentTime
        finalStatus <- case treeResult of
          Left e -> return $ Failure (Reason Nothing "Subtree failed")
          Right _ -> do
            subTreeResults <- forM subtree $ \t -> do
              (runTreeLabel t, ) <$> (readTVarIO $ runTreeStatus t)

            -- Check that the tree is actually done running
            let notDoneResults = L.filter (not . isDone . snd) subTreeResults
            unless (L.null notDoneResults) $ throwIO $ userError [i|Some tree nodes were not done: #{fmap fst notDoneResults}|]

            return $ case L.length (L.filter (isFailure . (\(_, Done _ _ r) -> r)) subTreeResults) of
              0 -> case L.length (L.filter (isPending . (\(_, Done _ _ r) -> r)) subTreeResults) of
                0 -> Success
                1 -> Failure (Pending Nothing (Just [i|1 child pending|]))
                n -> Failure (Pending Nothing (Just [i|#{n} children pending|]))
              1 -> Failure (Reason Nothing [i|1 child failed|])
              n -> Failure (Reason Nothing [i|#{n} children failed|])
        atomically $ writeTVar status $ Done startTime endTime finalStatus

  continueWith tree next

-- * Helpers

runTreeSequentially :: (HasBaseContext context) => Free (SpecCommand context IO) () -> ReaderT (RunTreeContext context) IO [RunTree]
runTreeSequentially spec = do
  (_, _, _, rtc@RunTreeContext {..}) <- getInfo

  let immediateChildren = getImmediateChildren spec
  (mconcat -> subtree) <- flip evalStateT runTreeContext $
    forM (L.zip immediateChildren [0..]) $ \(child, i) -> do
      contextAsync <- get

      tree <- lift $ withReaderT (const $ rtc { runTreeContext = contextAsync
                                              , runTreeIndexInParent = i
                                              , runTreeNumSiblings = L.length immediateChildren}) $ runTree child
      put =<< liftIO (async (waitForTree tree >> wait runTreeContext))
      return tree

  return subtree

continueWith :: HasBaseContext context => RunTree -> SpecFree context IO r -> ReaderT (RunTreeContext context) IO [RunTree]
continueWith tree next = do
  rest <- local (\rtc@(RunTreeContext {..}) -> rtc { runTreeIndexInParent = runTreeIndexInParent + 1}) $ runTree next
  return (tree : rest)

recordExceptionInStatus :: TVar Status -> SomeException -> IO ()
recordExceptionInStatus status e = do
  endTime <- getCurrentTime
  let ret = case fromException e of
        Just (e' :: SomeAsyncException) -> Failure (GotAsyncException Nothing Nothing (SomeAsyncExceptionWithEq e'))
        _ -> case fromException e of
          Just (e' :: FailureReason) -> Failure e'
          _ -> Failure (GotException Nothing Nothing (SomeExceptionWithEq e))
  atomically $ modifyTVar status $ \case
    Running startTime -> Done startTime endTime ret
    _ -> Done endTime endTime ret

getInfo = do
  status <- liftIO $ newTVarIO NotStarted
  logs <- liftIO $ newTVarIO mempty
  toggled <- liftIO $ newTVarIO False
  rts <- ask
  return (status, logs, toggled, rts)

waitAndWrapContextException :: Async b -> IO b
waitAndWrapContextException contextAsync = do
  (tryAny $ wait contextAsync) >>= \case
    Left e -> throwIO $ GetContextException Nothing (SomeExceptionWithEq e)
    Right ctx -> return ctx

runExampleM :: HasBaseContext r => ExampleM r () -> r -> TVar (Seq LogEntry) -> Maybe String -> IO Result
runExampleM ex ctx logs exceptionMessage = runExampleM' ex ctx logs exceptionMessage >>= \case
  Left err -> return $ Failure err
  Right () -> return Success

runExampleM' :: HasBaseContext r => ExampleM r a -> r -> TVar (Seq LogEntry) -> Maybe String -> IO (Either FailureReason a)
runExampleM' ex ctx logs exceptionMessage = do
  maybeTestDirectory <- getTestDirectory ctx
  let options = baseContextOptions $ getBaseContext ctx

  handleAny (wrapInFailureReasonIfNecessary exceptionMessage) $
    withLogFn maybeTestDirectory options $ \logFn ->
      (Right <$> (runLoggingT (runReaderT (unExampleT ex) ctx) logFn))

  where
    withLogFn :: Maybe FilePath -> Options -> (LogFn -> IO a) -> IO a
    withLogFn Nothing (Options {..}) action = action (logToMemory optionsSavedLogLevel logs)
    withLogFn (Just logPath) (Options {..}) action = withFile (logPath </> "test_logs.txt") AppendMode $ \h -> do
      hSetBuffering h LineBuffering
      action (logToMemoryAndFile optionsMemoryLogLevel optionsSavedLogLevel logs h)

    getTestDirectory :: (HasBaseContext a) => a -> IO (Maybe FilePath)
    getTestDirectory (getBaseContext -> (BaseContext {..})) = case baseContextRunRoot of
      Nothing -> return Nothing
      Just base -> do
        let dir = foldl (</>) base (fmap pathSegmentToName baseContextPath)
        createDirectoryIfMissing True dir
        return $ Just dir

wrapInFailureReasonIfNecessary :: Maybe String -> SomeException -> IO (Either FailureReason a)
wrapInFailureReasonIfNecessary exceptionMessage e = return $ Left $ case fromException e of
  Just (x :: FailureReason) -> x
  _ -> GotException Nothing exceptionMessage (SomeExceptionWithEq e)

addPathSegment pathSegment ctx = modifyBaseContext ctx (\x@(BaseContext {..}) -> x { baseContextPath = baseContextPath |> pathSegment  })

-- | FOR DEBUGGING ONLY
logMsg logs msg = do
  now <- getCurrentTime
  atomically $ modifyTVar logs (|> LogEntry now (Loc "" "" "" (0, 0) (0, 0)) "asdf" LevelDebug msg)
