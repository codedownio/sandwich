{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

module Test.Sandwich.Interpreters.StartTree (
  startTree
  , runNodesSequentially
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
import Data.String.Interpolate
import qualified Data.Text as T
import Data.Time.Clock
import Data.Typeable
import GHC.Stack
import System.Directory
import System.FilePath
import System.IO
import Test.Sandwich.Formatters.Print
import Test.Sandwich.Formatters.Print.CallStacks
import Test.Sandwich.Formatters.Print.FailureReason
import Test.Sandwich.Formatters.Print.Logs
import Test.Sandwich.Formatters.Print.Printing
import Test.Sandwich.Interpreters.RunTree.Logging
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.RunTree
import Test.Sandwich.TestTimer
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Test.Sandwich.Types.TestTimer
import Test.Sandwich.Util


baseContextFromCommon :: RunNodeCommonWithStatus s l t -> BaseContext -> BaseContext
baseContextFromCommon (RunNodeCommonWithStatus {..}) bc@(BaseContext {}) =
  bc { baseContextPath = runTreeFolder }

startTree :: (MonadIO m, HasBaseContext context) => RunNode context -> context -> m (Async Result)
startTree node@(RunNodeBefore {..}) ctx' = do
  let RunNodeCommonWithStatus {..} = runNodeCommon
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  runInAsync node ctx $ do
    (runExampleM runNodeBefore ctx runTreeLogs (Just [i|Exception in before '#{runTreeLabel}' handler|])) >>= \case
      result@(Failure fr) -> do
        markAllChildrenWithResult runNodeChildren ctx (Failure $ GetContextException Nothing (SomeExceptionWithEq $ toException fr))
        return result
      Success -> do
        void $ runNodesSequentially runNodeChildren ctx
        return Success
startTree node@(RunNodeAfter {..}) ctx' = do
  let RunNodeCommonWithStatus {..} = runNodeCommon
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  runInAsync node ctx $ do
    result <- liftIO $ newIORef Success
    finally (void $ runNodesSequentially runNodeChildren ctx)
            ((runExampleM runNodeAfter ctx runTreeLogs (Just [i|Exception in after '#{runTreeLabel}' handler|])) >>= writeIORef result)
    liftIO $ readIORef result
startTree node@(RunNodeIntroduce {..}) ctx' = do
  let RunNodeCommonWithStatus {..} = runNodeCommon
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  runInAsync node ctx $ do
    result <- liftIO $ newIORef Success
    bracket (do
                let asyncExceptionResult e = Failure $ GotAsyncException Nothing (Just [i|introduceWith #{runTreeLabel} alloc handler got async exception|]) (SomeAsyncExceptionWithEq e)
                flip withException (\(e :: SomeAsyncException) -> markAllChildrenWithResult runNodeChildrenAugmented ctx (asyncExceptionResult e)) $
                  runExampleM' runNodeAlloc ctx runTreeLogs (Just [i|Failure in introduce '#{runTreeLabel}' allocation handler|]))
            (\case
                Left failureReason -> writeIORef result (Failure failureReason)
                Right intro ->
                  (runExampleM (runNodeCleanup intro) ctx runTreeLogs (Just [i|Failure in introduce '#{runTreeLabel}' cleanup handler|])) >>= writeIORef result
            )
            (\case
                Left failureReason -> do
                  -- TODO: add note about failure in allocation
                  markAllChildrenWithResult runNodeChildrenAugmented ctx (Failure $ GetContextException Nothing (SomeExceptionWithEq $ toException failureReason))
                Right intro -> do
                  -- Special hack to modify the test timer profile via an introduce, without needing to track it everywhere.
                  -- It would be better to track the profile at the type level
                  let ctxFinal = case cast intro of
                        Just (TestTimerProfile t) -> modifyBaseContext ctx (\bc -> bc { baseContextTestTimerProfile = t })
                        Nothing -> ctx

                  void $ runNodesSequentially runNodeChildrenAugmented ((LabelValue intro) :> ctxFinal)
            )
    readIORef result
startTree node@(RunNodeIntroduceWith {..}) ctx' = do
  let RunNodeCommonWithStatus {..} = runNodeCommon
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  didRunWrappedAction <- liftIO $ newIORef (Left ())
  runInAsync node ctx $ do
    let wrappedAction = do
          let failureResult _e = Failure $ Reason Nothing [i|introduceWith '#{runTreeLabel}' handler threw exception|]
          flip withException (\e -> recordExceptionInStatus runTreeStatus e >> markAllChildrenWithResult runNodeChildrenAugmented ctx (failureResult e)) $ do
            runNodeIntroduceAction $ \intro -> do
              results <- liftIO $ runNodesSequentially runNodeChildrenAugmented ((LabelValue intro) :> ctx)
              liftIO $ writeIORef didRunWrappedAction (Right results)
              return results

          (liftIO $ readIORef didRunWrappedAction) >>= \case
            Left () -> return $ Failure $ Reason Nothing [i|introduceWith '#{runTreeLabel}' handler didn't call action|]
            Right _ -> return Success
    runExampleM'' wrappedAction ctx runTreeLogs (Just [i|Exception in introduceWith '#{runTreeLabel}' handler|])
startTree node@(RunNodeAround {..}) ctx' = do
  let RunNodeCommonWithStatus {..} = runNodeCommon
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  didRunWrappedAction <- liftIO $ newIORef (Left ())
  runInAsync node ctx $ do
    let wrappedAction = do
          let failureResult _e = Failure $ Reason Nothing [i|around #{runTreeLabel} handler threw exception|]
          flip withException (\e -> recordExceptionInStatus runTreeStatus e >> markAllChildrenWithResult runNodeChildren ctx (failureResult e)) $ do
            runNodeActionWith $ do
              results <- liftIO $ runNodesSequentially runNodeChildren ctx
              liftIO $ writeIORef didRunWrappedAction (Right results)
              return results

          (liftIO $ readIORef didRunWrappedAction) >>= \case
            Left () -> return $ Failure $ Reason Nothing [i|introduceWith '#{runTreeLabel}' handler didn't call action|]
            Right _ -> return Success
    runExampleM'' wrappedAction ctx runTreeLogs (Just [i|Exception in introduceWith '#{runTreeLabel}' handler|])
startTree node@(RunNodeDescribe {..}) ctx' = do
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  runInAsync node ctx $ do
    ((L.length . L.filter isFailure) <$> runNodesSequentially runNodeChildren ctx) >>= \case
      0 -> return Success
      n -> return $ Failure (Reason Nothing [i|#{n} #{if n == 1 then ("child" :: T.Text) else "children"} failed|])
startTree node@(RunNodeParallel {..}) ctx' = do
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  runInAsync node ctx $ do
    ((L.length . L.filter isFailure) <$> runNodesConcurrently runNodeChildren ctx) >>= \case
      0 -> return Success
      n -> return $ Failure (Reason Nothing [i|#{n} #{if n == 1 then ("child" :: T.Text) else "children"} failed|])
startTree node@(RunNodeIt {..}) ctx' = do
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  runInAsync node ctx $ do
    runExampleM runNodeExample ctx (runTreeLogs runNodeCommon) Nothing

-- * Util

runInAsync :: (HasBaseContext context, MonadIO m) => RunNode context -> context -> IO Result -> m (Async Result)
runInAsync node ctx action = do
  let RunNodeCommonWithStatus {..} = runNodeCommon node
  let bc@(BaseContext {..}) = getBaseContext ctx
  let timerFn = case runTreeRecordTime of
        True -> timeAction' (getTestTimer bc) baseContextTestTimerProfile (T.pack runTreeLabel)
        _ -> id
  startTime <- liftIO getCurrentTime
  mvar <- liftIO newEmptyMVar
  myAsync <- liftIO $ asyncWithUnmask $ \unmask -> do
    flip withException (recordExceptionInStatus runTreeStatus) $ unmask $ do
      readMVar mvar
      result <- timerFn action
      endTime <- liftIO getCurrentTime
      liftIO $ atomically $ writeTVar runTreeStatus $ Done startTime endTime result

      whenFailure result $ \reason -> do


        -- Create error symlink when configured to
        case node of
          RunNodeDescribe {} -> return () -- These are just noisy so don't create them
          RunNodeParallel {} -> return () -- These are just noisy so don't create them
          _ -> do
            whenJust baseContextErrorSymlinksDir $ \errorsDir ->
              whenJust baseContextPath $ \dir -> do
                whenJust baseContextRunRoot $ \runRoot -> do
                  -- Get a relative path from the error dir to the results dir. System.FilePath doesn't want to
                  -- introduce ".." components, so we have to do it ourselves
                  let errorDirDepth = L.length $ splitPath $ makeRelative runRoot errorsDir
                  let relativePath = joinPath (L.replicate errorDirDepth "..") </> (makeRelative runRoot dir)

                  let symlinkBaseName = case runTreeLoc of
                        Nothing -> takeFileName dir
                        Just loc -> [i|#{srcLocFile loc}:#{srcLocStartLine loc}_#{takeFileName dir}|]
                  let symlinkPath = errorsDir </> (nodeToFolderName symlinkBaseName 9999999 runTreeId)

                  -- Delete the symlink if it's already present. This can happen when re-running
                  -- a previously failed test
                  exists <- doesPathExist symlinkPath
                  when exists $ removePathForcibly symlinkPath

                  liftIO $ createDirectoryLink relativePath symlinkPath

        -- Write failure info
        whenJust baseContextPath $ \dir -> do
          withFile (dir </> "failure.txt") AppendMode $ \h -> do
            -- Use the PrintFormatter to format failure.txt nicely
            let pf = defaultPrintFormatter {
                  printFormatterUseColor = False
                  , printFormatterLogLevel = Just LevelDebug
                  , printFormatterIncludeCallStacks = True
                  }
            flip runReaderT (pf, 0, h) $ do
              printFailureReason reason
              whenJust (failureCallStack reason) $ \cs -> do
                p "\n"
                printCallStack cs
              p "\n"
              printLogs runTreeLogs

      return result
  liftIO $ atomically $ writeTVar runTreeStatus $ Running startTime myAsync
  liftIO $ putMVar mvar ()
  return myAsync  -- TODO: fix race condition with writing to runTreeStatus (here and above)

-- | Run a list of children sequentially, cancelling everything on async exception TODO
runNodesSequentially :: HasBaseContext context => [RunNode context] -> context -> IO [Result]
runNodesSequentially children ctx =
  flip withException (\(e :: SomeAsyncException) -> cancelAllChildrenWith children e) $
    forM (L.filter (shouldRunChild ctx) children) $ \child ->
      startTree child ctx >>= wait

-- | Run a list of children sequentially, cancelling everything on async exception TODO
runNodesConcurrently :: HasBaseContext context => [RunNode context] -> context -> IO [Result]
runNodesConcurrently children ctx =
  flip withException (\(e :: SomeAsyncException) -> cancelAllChildrenWith children e) $
    mapM wait =<< sequence [startTree child ctx
                           | child <- L.filter (shouldRunChild ctx) children]

markAllChildrenWithResult :: (MonadIO m, HasBaseContext context') => [RunNode context] -> context' -> Result -> m ()
markAllChildrenWithResult children baseContext status = do
  now <- liftIO getCurrentTime
  forM_ (L.filter (shouldRunChild' baseContext) $ concatMap getCommons children) $ \child ->
    liftIO $ atomically $ writeTVar (runTreeStatus child) (Done now now status)

cancelAllChildrenWith :: [RunNode context] -> SomeAsyncException -> IO ()
cancelAllChildrenWith children e = do
  forM_ children $ \node ->
    readTVarIO (runTreeStatus $ runNodeCommon node) >>= \case
      Running {..} -> cancelWith statusAsync e
      NotStarted -> do
        now <- getCurrentTime
        let reason = GotAsyncException Nothing Nothing (SomeAsyncExceptionWithEq e)
        atomically $ writeTVar (runTreeStatus $ runNodeCommon node) (Done now now (Failure reason))
      _ -> return ()

shouldRunChild :: (HasBaseContext ctx) => ctx -> RunNodeWithStatus context s l t -> Bool
shouldRunChild ctx node = shouldRunChild' ctx (runNodeCommon node)

shouldRunChild' :: (HasBaseContext ctx) => ctx -> RunNodeCommonWithStatus s l t -> Bool
shouldRunChild' ctx common = case baseContextOnlyRunIds $ getBaseContext ctx of
  Nothing -> True
  Just ids -> (runTreeId common) `S.member` ids

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
      action (logToMemoryAndFile optionsMemoryLogLevel optionsSavedLogLevel optionsLogFormatter logs h)

    getTestDirectory :: (HasBaseContext a) => a -> IO (Maybe FilePath)
    getTestDirectory (getBaseContext -> (BaseContext {..})) = case baseContextPath of
      Nothing -> return Nothing
      Just dir -> do
        createDirectoryIfMissing True dir
        return $ Just dir

    wrapInFailureReasonIfNecessary :: Maybe String -> SomeException -> IO (Either FailureReason a)
    wrapInFailureReasonIfNecessary msg e = return $ Left $ case fromException e of
      Just (x :: FailureReason) -> x
      _ -> case fromException e of
        Just (SomeExceptionWithCallStack e cs) -> GotException (Just cs) msg (SomeExceptionWithEq (SomeException e))
        _ -> GotException Nothing msg (SomeExceptionWithEq e)

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
