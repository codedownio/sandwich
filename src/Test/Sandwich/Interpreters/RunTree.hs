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
import Data.Sequence as Seq hiding ((:>))
import Data.String.Interpolate
import Data.Time.Clock
import System.Directory
import System.FilePath
import System.IO
import Test.Sandwich.Interpreters.RunTree.Logging
import Test.Sandwich.Types.Options
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


waitForTree :: [RunTree] -> IO (Either SomeException ())
waitForTree rts = tryAny (mapM_ wait (fmap runTreeAsync rts))

data RunTreeContext context = RunTreeContext {
  runTreeContext :: Async context
  , runTreeOptions :: Options
  }

runTreeMain :: (HasBaseContext context) => Free (SpecCommand context) () -> ReaderT (RunTreeContext context) IO [RunTree]
runTreeMain spec = do
  [RunTreeGroup {..}] <- runTree (Free (Describe "implicit outer describe" spec (Pure ())))
  return runTreeChildren


runTree :: (HasBaseContext context) => Free (SpecCommand context) r -> ReaderT (RunTreeContext context) IO [RunTree]

runTree (Free (Before l f subspec next)) = do
  (status, logs, toggled, rtc@RunTreeContext {..}) <- getInfo

  newContextAsync <- liftIO $ asyncWithUnmask $ \unmask -> do
    flip withException (recordAsyncExceptionInStatus status) $ unmask $ do
      ctx <- wait runTreeContext
      startTime <- getCurrentTime
      atomically $ writeTVar status (Running startTime)
      eitherResult <- tryAny $ runExampleM (PathSegment l True) f ctx logs
      let ret = either (\e -> Failure (GotException (Just "Exception in before handler") (SomeExceptionWithEq e))) (const Success) eitherResult
      endTime <- getCurrentTime
      atomically $ writeTVar status (Done startTime endTime ret)

      case eitherResult of
        Left e -> throwIO e
        Right _ -> return ctx

  subtree <- withReaderT (const $ rtc { runTreeContext = newContextAsync }) $ runTree subspec

  myAsync <- liftIO $ liftIO $ asyncWithUnmask $ \unmask -> do
    flip withException (recordAsyncExceptionInStatus status) $ unmask $ do
      void $ waitForTree subtree

  continueWith (RunTreeGroup l toggled status True subtree logs myAsync) next
runTree (Free (After l f subspec next)) = do
  (status, logs, toggled, RunTreeContext {..}) <- getInfo

  subtree <- runTree subspec

  myAsync <- liftIO $ async $ do
    _ <- waitForTree subtree
    ctx <- wait runTreeContext
    startTime <- getCurrentTime
    atomically $ writeTVar status (Running startTime)
    eitherResult <- tryAny $ runExampleM (PathSegment l True) f ctx logs
    endTime <- getCurrentTime
    atomically $ writeTVar status $ Done startTime endTime $
      either (\e -> Failure (GotException (Just "Exception in after handler") (SomeExceptionWithEq e))) id eitherResult

  continueWith (RunTreeGroup l toggled status True subtree logs myAsync) next
runTree (Free (Around l f subspec next)) = do
  (status, logs, toggled, rtc@RunTreeContext {..}) <- getInfo

  -- Use mvar to control when subspec is allowed to run
  mvar :: MVar () <- liftIO newEmptyMVar
  newContextAsync <- liftIO $ async $ do
    ret <- wait runTreeContext
    void $ takeMVar mvar
    return ret

  subtree <- withReaderT (const $ rtc { runTreeContext = newContextAsync }) $ runTree subspec

  myAsync <- liftIO $ async $ do
    ctx <- wait runTreeContext
    startTime <- getCurrentTime
    atomically $ writeTVar status (Running startTime)

    let action = do
          putMVar mvar ()
          void $ waitForTree subtree

    eitherResult <- tryAny $ runExampleM (PathSegment l True) (f action) ctx logs

    endTime <- getCurrentTime

    atomically $ writeTVar status $ Done startTime endTime $ case eitherResult of
      Left e -> Failure $ GotException (Just "Exception in around handler") (SomeExceptionWithEq e)
      Right _ -> Success

  continueWith (RunTreeGroup l toggled status True subtree logs myAsync) next
runTree (Free (Introduce l cl alloc cleanup subspec next)) = do
  (status, logs, toggled, rtc@RunTreeContext {..}) <- getInfo

  mvar <- liftIO newEmptyMVar

  newContextAsync <- liftIO $ async $ takeMVar mvar

  subtree <- withReaderT (const $ rtc { runTreeContext = newContextAsync }) $ runTree subspec

  myAsync <- liftIO $ asyncWithUnmask $ \unmask -> do
    -- On any async exception, record it in our status and throw it to the context async
    -- (to ensure no child ends up waiting on it forever. the child may have already finished waiting on it at this point)
    flip withException (\e -> recordAsyncExceptionInStatus status e >> cancelWith newContextAsync e) $ unmask $ do
      bracket (do
                  ctx <- waitAndHandleContextExceptionRethrow runTreeContext status
                  startTime <- getCurrentTime
                  atomically $ writeTVar status (Running startTime)
                  eitherResult <- tryAny $ runExampleM' (PathSegment l True) alloc ctx logs
                  case eitherResult of
                    Right (Right x) -> do
                      let newCtx = LabelValue x :> ctx
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
                    Right newCtx -> do
                      eitherResult <- tryAny $ runExampleM (PathSegment l True) cleanup newCtx logs
                      return $ either (\e -> Failure (GotException (Just "Exception in cleanup handler") (SomeExceptionWithEq e))) id eitherResult
                    Left e -> return $ Failure e

                  endTime <- getCurrentTime
                  atomically $ modifyTVar status $ \case
                    Running startTime -> Done startTime endTime finalStatus
                    _ -> Done endTime endTime finalStatus
              )
              (\_ -> void $ waitForTree subtree)
  continueWith (RunTreeGroup l toggled status True subtree logs myAsync) next
runTree (Free (It l ex next)) = do
  (status, logs, toggled, RunTreeContext {..}) <- getInfo

  myAsync <- liftIO $ asyncWithUnmask $ \unmask -> do
    flip withException (recordAsyncExceptionInStatus status) $ unmask $ do
      (tryAny $ wait runTreeContext) >>= \case
        Left e -> do
          endTime <- getCurrentTime
          atomically $ writeTVar status $ Done endTime endTime $ Failure (GetContextException (SomeExceptionWithEq e))
        Right ctx -> do
          startTime <- getCurrentTime
          atomically $ writeTVar status (Running startTime)
          eitherResult <- tryAny $ runExampleM (PathSegment l False) ex ctx logs
          endTime <- getCurrentTime
          atomically $ writeTVar status $ Done startTime endTime $
            either (Failure . (GotException (Just "Unknown exception") . SomeExceptionWithEq)) id eitherResult
  continueWith (RunTreeSingle l toggled status logs myAsync) next
runTree (Free (Describe l subspec next)) = runDescribe False l subspec next
runTree (Free (DescribeParallel l subspec next)) = runDescribe True l subspec next
runTree (Pure _) = return []


runDescribe :: (HasBaseContext context) => Bool -> String -> Spec context () -> Spec context r -> ReaderT (RunTreeContext context) IO [RunTree]
runDescribe parallel l subspec next = do
  (status, logs, toggled, rtc@RunTreeContext {..}) <- getInfo

  (mconcat -> subtree) <- flip evalStateT runTreeContext $
    forM (getImmediateChildren subspec) $ \child -> do
      contextAsync <- get
      let asyncToUse = if parallel then runTreeContext else contextAsync

      asyncWithPathSegment <- liftIO $ async $ do
        ctx <- wait asyncToUse
        let pathSegment = PathSegment l False
        return $ modifyBaseContext ctx (\x@(BaseContext {..}) -> x { baseContextPath = baseContextPath |> pathSegment })

      tree <- lift $ withReaderT (const $ rtc { runTreeContext = asyncWithPathSegment }) $ runTree child
      put =<< liftIO (async $ waitForTree tree >> wait runTreeContext)
      return tree

  myAsync <- liftIO $ asyncWithUnmask $ \unmask -> do
    flip withException (recordAsyncExceptionInStatus status) $ unmask $ do
      eitherContextResult <- tryAny $ wait runTreeContext
      startTime <- getCurrentTime
      atomically $ writeTVar status (Running startTime)
      _ <- waitForTree subtree
      endTime <- getCurrentTime
      finalStatus <- case eitherContextResult of
        Left e -> return $ Failure (GetContextException (SomeExceptionWithEq e))
        Right _ -> do
          subTreeResults <- forM subtree (readTVarIO . runTreeStatus)
          return $ case L.length (L.filter (isFailure . (\(Done _ _ r) -> r)) subTreeResults) of
            0 -> Success
            n -> Failure (Reason Nothing [i|#{n} children failed|])
      atomically $ writeTVar status $ Done startTime endTime finalStatus

  let tree = RunTreeGroup l toggled status False subtree logs myAsync
  rest <- runTree next
  return (tree : rest)


-- * Helpers

continueWith :: HasBaseContext context => RunTree -> SpecM context r -> ReaderT (RunTreeContext context) IO [RunTree]
continueWith tree next = do
  rest <- runTree next
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

logMsg :: TVar (Seq a) -> a -> IO ()
logMsg logs msg = atomically $ modifyTVar logs (|> msg)

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

runExampleM :: HasBaseContext r => PathSegment -> ExampleM r () -> r -> TVar (Seq LogEntry) -> IO Result
runExampleM pathSegment ex ctx logs = runExampleM' pathSegment ex ctx logs >>= \case
  Left err -> return $ Failure err
  Right () -> return Success

runExampleM' :: HasBaseContext r => PathSegment -> ExampleM r a -> r -> TVar (Seq LogEntry) -> IO (Either FailureReason a)
runExampleM' pathSegment ex ctx logs = do
  let ctx' = modifyBaseContext ctx (\x@(BaseContext {..}) -> x { baseContextPath = baseContextPath |> pathSegment  })

  maybeTestDirectory <- getTestDirectory ctx'
  let options = baseContextOptions $ getBaseContext ctx'

  withLogFn maybeTestDirectory options $ \logFn ->
    (runLoggingT (runExceptT $ runReaderT (unExampleM ex) ctx') logFn) >>= \case
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
        -- Note the drop 1 because of the implicit outer describe
        let dir = (foldl (</>) base (fmap (fixupPathSegmentName . pathSegmentName) $ Seq.drop 1 baseContextPath)) </> "results"
        createDirectoryIfMissing True dir
        return $ Just dir

    fixupPathSegmentName = replace '/' '_'

    replace :: Eq a => a -> a -> [a] -> [a]
    replace a b = map $ \c -> if c == a then b else c

getImmediateChildren :: Free (SpecCommand context) () -> [Free (SpecCommand context) ()]
getImmediateChildren (Free (It l ex next)) = (Free (It l ex (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Before l f subspec next)) = (Free (Before l f subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (After l f subspec next)) = (Free (After l f subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Introduce l cl alloc cleanup subspec next)) = (Free (Introduce l cl alloc cleanup subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Around l f subspec next)) = (Free (Around l f subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Describe l subspec next)) = (Free (Describe l subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (DescribeParallel l subspec next)) = (Free (DescribeParallel l subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Pure ()) = [Pure ()]
