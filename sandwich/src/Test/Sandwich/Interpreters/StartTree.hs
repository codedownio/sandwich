{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
-- |

module Test.Sandwich.Interpreters.StartTree (
  startTree
  , runNodesSequentially
  , StartTreeContext(..)
  ) where


import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.List as L
import Data.Sequence hiding ((:>))
import qualified Data.Set as S
import Data.String.Interpolate.IsString
import Data.Time.Clock
import System.Directory
import System.FilePath
import System.IO
import Test.Sandwich.Interpreters.RunTree.Logging
import Test.Sandwich.Types.Options
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


data StartTreeContext = StartTreeContext {
  startTreeOnlyRunIds :: Maybe (S.Set Int)
  }


baseContextFromCommon :: RunNodeCommonWithStatus s l t -> BaseContext -> BaseContext
baseContextFromCommon (RunNodeCommonWithStatus {..}) bc@(BaseContext {}) =
  bc { baseContextPath = runTreeFolder }

startTree :: (MonadIO m, HasBaseContext context) => RunNode context -> context -> ReaderT StartTreeContext m (Async Result)
startTree node@(RunNodeBefore {..}) ctx' = do
  let RunNodeCommonWithStatus {..} = runNodeCommon
  stc <- ask
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  runInAsync node $ do
    (runExampleM runNodeBefore ctx runTreeLogs (Just [i|Exception in before '#{runTreeLabel}' handler|])) >>= \case
      result@(Failure fr) -> do
        markAllChildrenWithStatus runNodeChildren stc (Failure $ GetContextException Nothing (SomeExceptionWithEq $ toException fr))
        return result
      Success -> do
        ((L.length . L.filter isFailure) <$> runNodesSequentially runNodeChildren ctx stc) >>= \case
          0 -> return Success
          n -> return $ Failure (Reason Nothing [i|#{n} #{if n == 1 then "child" else "children"} failed|])
startTree node@(RunNodeAfter {..}) ctx' = do
  let RunNodeCommonWithStatus {..} = runNodeCommon
  stc <- ask
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  runInAsync node $ do
    _ <- runNodesSequentially runNodeChildren ctx stc
    -- TODO: should we actually fail this if any children fail? Maybe not -- make before consistent
    runExampleM runNodeAfter ctx runTreeLogs (Just [i|Exception in after '#{runTreeLabel}' handler|])
startTree node@(RunNodeIntroduce {..}) ctx' = do
  let RunNodeCommonWithStatus {..} = runNodeCommon
  stc <- ask
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  runInAsync node $ do
    bracket (runExampleM' runNodeAlloc ctx runTreeLogs (Just [i|Failure in introduce '#{runTreeLabel}' allocation handler|]))
            (\case
                Left failureReason -> return $ Failure failureReason
                Right intro -> do
                  (runExampleM' (runNodeCleanup intro) ctx runTreeLogs (Just [i|Failure in introduce '#{runTreeLabel}' cleanup handler|])) >>= \case
                    Left failureReason -> return $ Failure failureReason
                    Right () -> return Success
            )
            (\case
                Left failureReason -> do
                  markAllChildrenWithStatus runNodeChildrenAugmented stc (Failure failureReason)
                  -- TODO: add note about failure in allocation
                  -- TODO: make sure cleanup failures can find their way to the status (bracket swallows this return value)
                  return $ Failure failureReason
                Right intro -> do
                  ((L.length . L.filter isFailure) <$> runNodesSequentially runNodeChildrenAugmented ((LabelValue intro) :> ctx) stc) >>= \case
                    0 -> return Success
                    n -> return $ Failure (Reason Nothing [i|#{n} #{if n == 1 then "child" else "children"} failed|])
            )
startTree node@(RunNodeIntroduceWith {..}) ctx' = do
  let RunNodeCommonWithStatus {..} = runNodeCommon
  stc <- ask
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  didRunWrappedAction <- liftIO $ newIORef (Left ())
  runInAsync node $ do
    let wrappedAction = do
          flip withException (\e -> recordExceptionInStatus runTreeStatus e >> markAllChildrenWithStatus runNodeChildrenAugmented stc (Failure $ Reason Nothing [i|introduceWith #{runTreeLabel} handler threw exception|])) $ do
            runNodeIntroduceAction $ \intro -> do
              results <- runNodesSequentially runNodeChildrenAugmented ((LabelValue intro) :> ctx) stc
              writeIORef didRunWrappedAction (Right results)

          (liftIO $ readIORef didRunWrappedAction) >>= \case
            Left () -> return $ Failure $ Reason Nothing [i|introduceWith '#{runTreeLabel}' handler didn't call action|]
            Right results -> return Success

    runExampleM'' wrappedAction ctx runTreeLogs (Just [i|Exception in introduceWith '#{runTreeLabel}' handler|])
startTree node@(RunNodeAround {..}) ctx' = do
  let RunNodeCommonWithStatus {..} = runNodeCommon
  stc <- ask
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  didRunWrappedAction <- liftIO $ newIORef (Left ())
  runInAsync node $ do
    let wrappedAction = do
          flip withException (\e -> recordExceptionInStatus runTreeStatus e >> markAllChildrenWithStatus runNodeChildren stc (Failure $ Reason Nothing [i|around #{runTreeLabel} handler threw exception|])) $ do
            runNodeActionWith $ do
              results <- liftIO $ runNodesSequentially runNodeChildren ctx stc
              liftIO $ writeIORef didRunWrappedAction (Right results)
              return Success

          (liftIO $ readIORef didRunWrappedAction) >>= \case
            Left () -> return $ Failure $ Reason Nothing [i|introduceWith '#{runTreeLabel}' handler didn't call action|]
            Right results -> return Success
    runExampleM'' wrappedAction ctx runTreeLogs (Just [i|Exception in introduceWith '#{runTreeLabel}' handler|])
startTree node@(RunNodeDescribe {..}) ctx' = do
  stc <- ask
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  runInAsync node $ do
    ((L.length . L.filter isFailure) <$> runNodesSequentially runNodeChildren ctx stc) >>= \case
      0 -> return Success
      n -> return $ Failure (Reason Nothing [i|#{n} #{if n == 1 then "child" else "children"} failed|])
startTree node@(RunNodeParallel {..}) ctx' = do
  stc <- ask
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  runInAsync node $ do
    ((L.length . L.filter isFailure) <$> runNodesConcurrently runNodeChildren ctx stc) >>= \case
      0 -> return Success
      n -> return $ Failure (Reason Nothing [i|#{n} #{if n == 1 then "child" else "children"} failed|])
startTree node@(RunNodeIt {..}) ctx' = do
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  runInAsync node $ do
    runExampleM runNodeExample ctx (runTreeLogs runNodeCommon) Nothing

-- * Util

runInAsync :: MonadIO m => RunNode context -> IO Result -> m (Async Result)
runInAsync node action = do
  let RunNodeCommonWithStatus {..} = runNodeCommon node
  startTime <- liftIO getCurrentTime
  mvar <- liftIO newEmptyMVar
  myAsync <- liftIO $ async $ do
    readMVar mvar
    result <- action
    endTime <- liftIO getCurrentTime
    liftIO $ atomically $ writeTVar runTreeStatus $ Done startTime endTime result
    return result
  liftIO $ atomically $ writeTVar runTreeStatus $ Running startTime myAsync
  liftIO $ putMVar mvar ()
  return myAsync  -- TODO: fix race condition with writing to runTreeStatus (here and above)

-- | Run a list of children sequentially, cancelling everything on async exception TODO
runNodesSequentially :: HasBaseContext context => [RunNode context] -> context -> StartTreeContext -> IO [Result]
runNodesSequentially children ctx stc = do
  forM (L.filter (shouldRunChild stc) children) $ \child ->
    runReaderT (startTree child ctx) stc >>= wait

-- | Run a list of children sequentially, cancelling everything on async exception TODO
runNodesConcurrently :: HasBaseContext context => [RunNode context] -> context -> StartTreeContext -> IO [Result]
runNodesConcurrently children ctx stc = do
  mapM wait =<< sequence [runReaderT (startTree child ctx) stc
                         | child <- L.filter (shouldRunChild stc) children]

markAllChildrenWithStatus :: (MonadIO m) => [RunNode context] -> StartTreeContext -> Result -> m ()
markAllChildrenWithStatus children stc status = do
  now <- liftIO $ getCurrentTime
  forM_ children $ \child -> do
    case shouldRunChild stc child of
      True -> liftIO $ atomically $ writeTVar (runTreeStatus $ runNodeCommon child) (Done now now status)
      False -> return ()

shouldRunChild :: StartTreeContext -> RunNodeWithStatus context s l t -> Bool
shouldRunChild (StartTreeContext {..}) node = case startTreeOnlyRunIds of
  Nothing -> True
  Just ids -> (runTreeId $ runNodeCommon node) `S.member` ids

-- * Running examples

runExampleM :: HasBaseContext r => ExampleM r () -> r -> TVar (Seq LogEntry) -> Maybe String -> IO Result
runExampleM ex ctx logs exceptionMessage = runExampleM' ex ctx logs exceptionMessage >>= \case
  Left err -> return $ Failure err
  Right () -> return Success

runExampleM'' :: HasBaseContext r => ExampleM r Result -> r -> TVar (Seq LogEntry) -> Maybe String -> IO Result
runExampleM'' ex ctx logs exceptionMessage = runExampleM' ex ctx logs exceptionMessage >>= \case
  Left err -> return $ Failure err
  Right x -> return x

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
    getTestDirectory (getBaseContext -> (BaseContext {..})) = case baseContextPath of
      Nothing -> return Nothing
      Just dir -> do
        createDirectoryIfMissing True dir
        return $ Just dir

wrapInFailureReasonIfNecessary :: Maybe String -> SomeException -> IO (Either FailureReason a)
wrapInFailureReasonIfNecessary exceptionMessage e = return $ Left $ case fromException e of
  Just (x :: FailureReason) -> x
  _ -> GotException Nothing exceptionMessage (SomeExceptionWithEq e)

recordExceptionInStatus :: (MonadIO m) => TVar Status -> SomeException -> m ()
recordExceptionInStatus status e = do
  endTime <- liftIO getCurrentTime
  let ret = case fromException e of
        Just (e' :: SomeAsyncException) -> Failure (GotAsyncException Nothing Nothing (SomeAsyncExceptionWithEq e'))
        _ -> case fromException e of
          Just (e' :: FailureReason) -> Failure e'
          _ -> Failure (GotException Nothing Nothing (SomeExceptionWithEq e))
  liftIO $ atomically $ modifyTVar status $ \case
    Running {statusStartTime} -> Done statusStartTime endTime ret
    _ -> Done endTime endTime ret
