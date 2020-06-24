{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}
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


runTreeMain :: BaseContext -> Free (SpecCommand BaseContext) () -> IO [RunTree]
runTreeMain baseContext spec = do
  asyncBaseContext <- async $ return baseContext
  runReaderT (runTreeSequentially spec) $ RunTreeContext {
    runTreeContext = asyncBaseContext
    , runTreeOptions = baseContextOptions baseContext
    , runTreeIndexInParent = 0
    , runTreeNumSiblings = countChildren spec
    , runTreeCurrentFolder = baseContextRunRoot baseContext
    }

runTree :: (HasBaseContext context) => Free (SpecCommand context) r -> ReaderT (RunTreeContext context) IO [RunTree]
runTree (Free (Before l f subspec next)) = do
  (status, logs, toggled, rtc@RunTreeContext {..}) <- getInfo

  newContextAsync <- liftIO $ asyncWithUnmask $ \unmask -> do
    flip withException (recordAsyncExceptionInStatus status) $ unmask $ do
      (addPathSegment (PathSegment l True runTreeIndexInParent runTreeNumSiblings) -> ctx) <- wait runTreeContext
      startTime <- getCurrentTime
      atomically $ writeTVar status (Running startTime)
      eitherResult <- tryAny $ runExampleM f ctx logs
      let ret = either (\e -> Failure (GotException (Just "Exception in before handler") (SomeExceptionWithEq e))) (const Success) eitherResult
      endTime <- getCurrentTime
      atomically $ writeTVar status (Done startTime endTime ret)
      case eitherResult of
        Left e -> throwIO e
        Right _ -> return ctx

  subtree <- local (const $ rtc { runTreeContext = newContextAsync
                                , runTreeCurrentFolder = appendFolder rtc l }) $ runTreeSequentially subspec

  myAsync <- liftIO $ liftIO $ asyncWithUnmask $ \unmask -> do
    flip withException (recordAsyncExceptionInStatus status) $ unmask $ do
      void $ waitForTree subtree

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
    eitherResult <- tryAny $ runExampleM f ctx logs
    endTime <- getCurrentTime
    atomically $ writeTVar status $ Done startTime endTime $
      either (\e -> Failure (GotException (Just "Exception in after handler") (SomeExceptionWithEq e))) id eitherResult

  continueWith (RunTreeGroup l toggled status (appendFolder rtc l) True subtree logs myAsync) next
runTree (Free (Around l f subspec next)) = do
  (status, logs, toggled, rtc@RunTreeContext {..}) <- getInfo

  -- Use mvar to control when subspec is allowed to run
  mvar :: MVar () <- liftIO newEmptyMVar
  newContextAsync <- liftIO $ async $ do
    ctx <- wait runTreeContext
    void $ readMVar mvar
    return (addPathSegment (PathSegment l True runTreeIndexInParent runTreeNumSiblings) ctx)

  subtree <- withReaderT (const $ rtc { runTreeContext = newContextAsync
                                      , runTreeCurrentFolder = appendFolder rtc l }) $ runTreeSequentially subspec

  myAsync <- liftIO $ async $ do
    (addPathSegment (PathSegment l True runTreeIndexInParent runTreeNumSiblings) -> ctx) <- wait runTreeContext
    startTime <- getCurrentTime
    atomically $ writeTVar status (Running startTime)
    let action = putMVar mvar () >> void (waitForTree subtree)
    eitherResult <- tryAny $ runExampleM (f action) ctx logs
    endTime <- getCurrentTime
    atomically $ writeTVar status $ Done startTime endTime $ case eitherResult of
      Left e -> Failure $ GotException (Just "Exception in around handler") (SomeExceptionWithEq e)
      Right _ -> Success

  continueWith (RunTreeGroup l toggled status (appendFolder rtc l) True subtree logs myAsync) next
runTree (Free (Introduce l cl alloc cleanup subspec next)) = do
  (status, logs, toggled, rtc@RunTreeContext {..}) <- getInfo

  mvar <- liftIO newEmptyMVar
  newContextAsync <- liftIO $ async $ readMVar mvar

  subtree <- withReaderT (const $ rtc { runTreeContext = newContextAsync }) $ runTreeSequentially subspec

  myAsync <- liftIO $ asyncWithUnmask $ \unmask -> do
    -- On any async exception, record it in our status and throw it to the context async
    -- (to ensure no child ends up waiting on it forever. the child may have already finished waiting on it at this point)
    flip withException (\e -> recordAsyncExceptionInStatus status e >> cancelWith newContextAsync e) $ unmask $ do
      bracket (do
                  (addPathSegment (PathSegment l True runTreeIndexInParent runTreeNumSiblings) -> ctx) <-
                    waitAndHandleContextExceptionRethrow runTreeContext status
                  startTime <- getCurrentTime
                  atomically $ writeTVar status (Running startTime)
                  eitherResult <- tryAny $ runExampleM' alloc ctx logs
                  case eitherResult of
                    Right (Right x) -> do
                      let newCtx = LabelValue x :> ctx -- TODO: modify base context here to prepend path segment?
                      putMVar mvar newCtx
                      return $ Right newCtx
                    Right (Left err') -> do -- Alloc ExceptT had some failure
                      let err = GotException (Just "Exception in allocation handler") (SomeExceptionWithEq $ SomeException err')
                      cancelWith newContextAsync err
                      return $ Left err
                    Left err' -> do -- Exception while running alloc
                      let err = GotException (Just "Exception in allocation handler") (SomeExceptionWithEq $ SomeException err')
                      cancelWith newContextAsync err
                      return $ Left err
              )
              (\eitherStatus -> do
                  finalStatus <- case eitherStatus of
                    Right (LabelValue intro :> ctx) -> do
                      eitherResult <- tryAny $ runExampleM (cleanup intro) ctx logs
                      return $ either (\e -> Failure (GotException (Just "Exception in cleanup handler") (SomeExceptionWithEq e))) id eitherResult
                    Left e -> return $ Failure e

                  endTime <- getCurrentTime
                  atomically $ modifyTVar status $ \case
                    Running startTime -> Done startTime endTime finalStatus
                    _ -> Done endTime endTime finalStatus
              )
              (\_ -> void $ waitForTree subtree)
  continueWith (RunTreeGroup l toggled status (appendFolder rtc l) True subtree logs myAsync) next
runTree (Free (IntroduceWith l cl action subspec next)) = do
  (status, logs, toggled, rtc@RunTreeContext {..}) <- getInfo

  mvar <- liftIO newEmptyMVar
  newContextAsync <- liftIO $ async $ readMVar mvar
  subtree <- withReaderT (const $ rtc { runTreeContext = newContextAsync
                                      , runTreeCurrentFolder = appendFolder rtc l }) $ runTreeSequentially subspec

  myAsync <- liftIO $ asyncWithUnmask $ \unmask ->
    flip withException (\e -> recordAsyncExceptionInStatus status e >> cancelWith newContextAsync e) $ unmask $ do
      (addPathSegment (PathSegment l False runTreeIndexInParent runTreeNumSiblings) -> ctx) <- waitAndHandleContextExceptionRethrow runTreeContext status
      startTime <- liftIO getCurrentTime

      let wrappedAction = do
            liftIO $ atomically $ writeTVar status (Running startTime)

            let cleanup = liftIO $ do
                  contents <- tryReadMVar mvar
                  when (isNothing contents) $
                    -- The action failed to produce a value; cancel newContextAsync with an exception
                    cancelWith newContextAsync $ Reason Nothing "Action in introduceWith failed to produce a value"

            eitherResult <- tryAny $ flip finally cleanup $ action $ \x -> do
              liftIO $ putMVar mvar (LabelValue x :> ctx)
              void $ waitForTree subtree

            endTime <- liftIO getCurrentTime
            liftIO $ atomically $ writeTVar status $ Done startTime endTime $ case eitherResult of
              Left e -> Failure (GotException (Just "Unknown exception") (SomeExceptionWithEq e))
              Right _ -> Success

      result <- tryAny $ runExampleM wrappedAction ctx logs
      whenLeft result $ \e -> do
        endTime <- liftIO getCurrentTime
        let ret = GotException (Just "Unknown exception") (SomeExceptionWithEq e)
        atomically $ writeTVar status $ Done startTime endTime (Failure ret)
        cancelWith newContextAsync ret

  continueWith (RunTreeGroup l toggled status (appendFolder rtc l) True subtree logs myAsync) next
runTree (Free (It l ex next)) = do
  (status, logs, toggled, rtc@RunTreeContext {..}) <- getInfo

  myAsync <- liftIO $ asyncWithUnmask $ \unmask -> do
    flip withException (recordAsyncExceptionInStatus status) $ unmask $ do
      (tryAny $ wait runTreeContext) >>= \case
        Left e -> do
          endTime <- getCurrentTime
          atomically $ writeTVar status $ Done endTime endTime $ Failure (GetContextException (SomeExceptionWithEq e))
        Right (addPathSegment (PathSegment l False runTreeIndexInParent runTreeNumSiblings) -> ctx) -> do
          startTime <- getCurrentTime
          atomically $ writeTVar status (Running startTime)
          eitherResult <- tryAny $ runExampleM ex ctx logs
          endTime <- getCurrentTime
          atomically $ writeTVar status $ Done startTime endTime $
            either (Failure . (GotException (Just "Unknown exception") . SomeExceptionWithEq)) id eitherResult
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
                                , runTreeCurrentFolder = appendFolder rtc l }) $ runTree subspec
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


runDescribe :: (HasBaseContext context) => Bool -> String -> [RunTree] -> Spec context r -> ReaderT (RunTreeContext context) IO [RunTree]
runDescribe isContextManager l subtree next = do
  (status, logs, toggled, rtc@RunTreeContext {..}) <- getInfo

  tree <- RunTreeGroup l toggled status (appendFolder rtc l) isContextManager subtree logs <$> do
    liftIO $ asyncWithUnmask $ \unmask -> do
      flip withException (recordAsyncExceptionInStatus status) $ unmask $ do
        ctx <- waitAndHandleContextExceptionRethrow runTreeContext status
        startTime <- getCurrentTime
        atomically $ writeTVar status (Running startTime)
        treeResult <- waitForTree subtree
        endTime <- getCurrentTime
        finalStatus <- case treeResult of
          Left e -> return $ Failure (GetContextException (SomeExceptionWithEq e))
          Right _ -> do
            subTreeResults <- forM subtree (readTVarIO . runTreeStatus)
            return $ case L.length (L.filter (isFailure . (\(Done _ _ r) -> r)) subTreeResults) of
              0 -> Success
              1 -> Failure (Reason Nothing [i|1 child failed|])
              n -> Failure (Reason Nothing [i|#{n} children failed|])
        atomically $ writeTVar status $ Done startTime endTime finalStatus

  continueWith tree next

-- * Helpers

runTreeSequentially :: (HasBaseContext context) => Free (SpecCommand context) () -> ReaderT (RunTreeContext context) IO [RunTree]
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

continueWith :: HasBaseContext context => RunTree -> SpecM context r -> ReaderT (RunTreeContext context) IO [RunTree]
continueWith tree next = do
  rest <- local (\rtc@(RunTreeContext {..}) -> rtc { runTreeIndexInParent = runTreeIndexInParent + 1}) $ runTree next
  return (tree : rest)

recordAsyncExceptionInStatus :: TVar Status -> SomeAsyncException -> IO Result
recordAsyncExceptionInStatus status e = do
  endTime <- getCurrentTime
  let ret = Failure (GotAsyncException Nothing (SomeAsyncExceptionWithEq e))
  atomically $ modifyTVar status $ \case
    Running startTime -> Done startTime endTime ret
    _ -> Done endTime endTime ret
  return ret

getInfo = do
  status <- liftIO $ newTVarIO NotStarted
  logs <- liftIO $ newTVarIO mempty
  toggled <- liftIO $ newTVarIO False
  rts <- ask
  return (status, logs, toggled, rts)

waitAndHandleContextExceptionRethrow :: Async b -> TVar Status -> IO b
waitAndHandleContextExceptionRethrow contextAsync status = do
  (tryAny $ wait contextAsync) >>= \case
    Left e -> do
      let ret = Failure (GetContextException (SomeExceptionWithEq e))
      endTime <- getCurrentTime
      atomically $ modifyTVar status $ \case
        Running startTime -> (Done startTime endTime ret) -- TODO: make startTime a Maybe
        _ -> (Done endTime endTime ret) -- TODO: make startTime a Maybe
      throwIO e
    Right ctx -> return ctx

runExampleM :: HasBaseContext r => ExampleM r () -> r -> TVar (Seq LogEntry) -> IO Result
runExampleM ex ctx logs = runExampleM' ex ctx logs >>= \case
  Left err -> return $ Failure err
  Right () -> return Success

runExampleM' :: HasBaseContext r => ExampleM r a -> r -> TVar (Seq LogEntry) -> IO (Either FailureReason a)
runExampleM' ex ctx logs = do
  maybeTestDirectory <- getTestDirectory ctx
  let options = baseContextOptions $ getBaseContext ctx

  withLogFn maybeTestDirectory options $ \logFn ->
    (runLoggingT (runExceptT $ runReaderT (unExampleT ex) ctx) logFn) >>= \case
      Left err -> return $ Left err
      Right x -> return $ Right x

  where
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

    pathSegmentToName (PathSegment {..}) = nodeToFolderName pathSegmentName pathSegmentNumSiblings pathSegmentIndexInParent

addPathSegment pathSegment ctx = modifyBaseContext ctx (\x@(BaseContext {..}) -> x { baseContextPath = baseContextPath |> pathSegment  })
