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


waitForTree :: [RunTree] -> IO (Either SomeException [Result])
waitForTree rts = tryAny (mapM wait (fmap runTreeAsync rts))

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
    flip withException (handleAsyncException status) $ unmask $ do
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
    flip withException (handleAsyncException status) $ unmask $ do
      _ <- tryAny $ mapM_ wait (fmap runTreeAsync subtree)
      atomically $ do
        (readTVar status) >>= \case
          NotStarted -> retry -- Should never happen
          Running {} -> retry -- Should never happen
          Done _ _ result -> return result

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
    let ret = either (\e -> Failure (GotException (Just "Exception in after handler") (SomeExceptionWithEq e))) id eitherResult
    atomically $ writeTVar status (Done startTime endTime ret)
    return ret

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

    let ret = case eitherResult of
          Left e -> Failure $ GotException (Just "Exception in around handler") (SomeExceptionWithEq e)
          Right _ -> Success

    atomically $ writeTVar status (Done startTime endTime Success)
    return ret

  continueWith (RunTreeGroup l toggled status True subtree logs myAsync) next
runTree (Free (Introduce l cl alloc cleanup subspec next)) = do
  (status, logs, toggled, rtc@RunTreeContext {..}) <- getInfo

  newContextAsync <- liftIO $ async $ do
    ctx <- waitOrHandleContextException runTreeContext status
    startTime <- getCurrentTime
    atomically $ writeTVar status (Running startTime)
    eitherResult <- tryAny $ runExampleM' (PathSegment l True) alloc ctx logs
    let ret = either (\e -> (Failure (GotException (Just "Exception in introduce allocate handler") (SomeExceptionWithEq e)))) (const Success) eitherResult
    endTime <- getCurrentTime
    atomically $ writeTVar status (Done startTime endTime ret)
    case eitherResult of
      Left e -> throwIO e -- caught an exception
      Right (Left e) -> throwIO e -- the ExampleM failed
      Right (Right intro) -> return (LabelValue intro :> ctx)

  subtree <- withReaderT (const $ rtc { runTreeContext = newContextAsync }) $ runTree subspec

  myAsync <- liftIO $ async $ do
    _ <- waitForTree subtree
    ctx <- waitOrHandleContextException newContextAsync status
    startTime <- getCurrentTime -- TODO
    eitherResult <- tryAny $ runExampleM (PathSegment l True) cleanup ctx logs
    endTime <- getCurrentTime
    let ret = either (\e -> Failure (GotException (Just "Exception in introduce cleanup handler") (SomeExceptionWithEq e))) id eitherResult
    atomically $ writeTVar status (Done startTime endTime ret)
    return ret

  continueWith (RunTreeGroup l toggled status True subtree logs myAsync) next
runTree (Free (It l ex next)) = do
  (status, logs, toggled, RunTreeContext {..}) <- getInfo

  myAsync <- liftIO $ asyncWithUnmask $ \unmask -> do
    flip withException (handleAsyncException status) $ unmask $ do
      ctx <- waitOrHandleContextException runTreeContext status
      startTime <- getCurrentTime
      atomically $ writeTVar status (Running startTime)
      eitherResult <- tryAny $ runExampleM (PathSegment l False) ex ctx logs
      endTime <- getCurrentTime
      let ret = either (Failure . (GotException (Just "Unknown exception") . SomeExceptionWithEq)) id eitherResult
      atomically $ writeTVar status (Done startTime endTime ret)
      return ret

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
    flip withException (handleAsyncException status) $ unmask $ do
      _ <- waitOrHandleContextException runTreeContext status

      startTime <- getCurrentTime
      atomically $ writeTVar status (Running startTime)
      eitherResults <- waitForTree subtree
      endTime <- getCurrentTime
      let ret = case eitherResults of
            Left e -> Failure (Reason Nothing "Child tree threw an exception")
            Right results -> case L.length (L.filter isFailure results) of
              0 -> Success
              n -> Failure (Reason Nothing [i|#{n} children failed|])
      atomically $ writeTVar status (Done startTime endTime ret)
      return ret

  let tree = RunTreeGroup l toggled status False subtree logs myAsync
  rest <- runTree next
  return (tree : rest)


-- * Helpers

continueWith tree next = do
  rest <- runTree next
  return (tree : rest)

handleAsyncException :: TVar Status -> SomeAsyncException -> IO Result
handleAsyncException status e = do
  -- TODO: get start time
  endTime <- getCurrentTime
  let ret = Failure (GotAsyncException Nothing (SomeAsyncExceptionWithEq e))
  atomically $ writeTVar status (Done endTime endTime ret)
  return ret

getInfo = do
  status <- liftIO $ newTVarIO NotStarted
  logs <- liftIO $ newTVarIO mempty
  toggled <- liftIO $ newTVarIO False
  rts <- ask
  return (status, logs, toggled, rts)

logMsg logs msg = atomically $ modifyTVar logs (|> msg)

waitOrHandleContextException contextAsync status = do
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
