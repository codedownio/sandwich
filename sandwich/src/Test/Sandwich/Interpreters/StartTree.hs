{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}

module Test.Sandwich.Interpreters.StartTree (
  startTree
  , runNodesSequentially
  , markAllChildrenWithResult
  ) where


import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
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
import UnliftIO.Exception
import UnliftIO.STM


baseContextFromCommon :: RunNodeCommonWithStatus s l t -> BaseContext -> BaseContext
baseContextFromCommon (RunNodeCommonWithStatus {..}) bc@(BaseContext {}) =
  bc { baseContextPath = runTreeFolder }

startTree :: (MonadIO m, HasBaseContext context) => RunNode context -> context -> m (Async Result)
startTree node@(RunNodeBefore {..}) ctx' = do
  let RunNodeCommonWithStatus {..} = runNodeCommon
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  runInAsync node ctx $ do
    (timed runTreeRecordTime (getBaseContext ctx) (runTreeLabel <> " (setup)") (runExampleM runNodeBefore ctx runTreeLogs (Just [i|Exception in before '#{runTreeLabel}' handler|]))) >>= \case
      (result@(Failure fr@(Pending {})), _setupStartTime, setupFinishTime) -> do
        markAllChildrenWithResult runNodeChildren ctx (Failure fr)
        return (result, mkSetupTimingInfo setupFinishTime)
      (result@(Failure fr), _setupStartTime, setupFinishTime) -> do
        markAllChildrenWithResult runNodeChildren ctx (Failure $ GetContextException Nothing (SomeExceptionWithEq $ toException fr))
        return (result, mkSetupTimingInfo setupFinishTime)
      (Success, _setupStartTime, setupFinishTime) -> do
        void $ runNodesSequentially runNodeChildren ctx
        return (Success, mkSetupTimingInfo setupFinishTime)
      (Cancelled, _setupStartTime, setupFinishTime) -> do
        return (Cancelled, mkSetupTimingInfo setupFinishTime)
      (DryRun, _setupStartTime, setupFinishTime) -> do
        void $ runNodesSequentially runNodeChildren ctx
        return (DryRun, mkSetupTimingInfo setupFinishTime)
startTree node@(RunNodeAfter {..}) ctx' = do
  let RunNodeCommonWithStatus {..} = runNodeCommon
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  runInAsync node ctx $ do
    result <- liftIO $ newIORef (Success, emptyExtraTimingInfo)
    finally (void $ runNodesSequentially runNodeChildren ctx)
            (do
                (ret, teardownStartTime, _teardownFinishTime) <- timed runTreeRecordTime (getBaseContext ctx) (runTreeLabel <> " (teardown)") $
                  runExampleM runNodeAfter ctx runTreeLogs (Just [i|Exception in after '#{runTreeLabel}' handler|])
                writeIORef result (ret, mkTeardownTimingInfo teardownStartTime)
            )
    liftIO $ readIORef result
startTree node@(RunNodeIntroduce {..}) ctx' = do
  let RunNodeCommonWithStatus {..} = runNodeCommon
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  runInAsync node ctx $ do
    result <- liftIO $ newIORef (Success, emptyExtraTimingInfo)
    bracket (do
                let asyncExceptionResult e = Failure $ GotAsyncException Nothing (Just [i|introduceWith #{runTreeLabel} alloc handler got async exception|]) (SomeAsyncExceptionWithEq e)
                flip withException (\(e :: SomeAsyncException) -> markAllChildrenWithResult runNodeChildrenAugmented ctx (asyncExceptionResult e)) $
                  timed runTreeRecordTime (getBaseContext ctx) (runTreeLabel <> " (setup)") $
                    runExampleM' runNodeAlloc ctx runTreeLogs (Just [i|Failure in introduce '#{runTreeLabel}' allocation handler|])
            )
            (\(ret, setupStartTime, setupFinishTime) -> case ret of
                Left failureReason -> writeIORef result (Failure failureReason, mkSetupTimingInfo setupStartTime)
                Right intro -> do
                  teardownStartTime <- getCurrentTime
                  addTeardownStartTimeToStatus runTreeStatus teardownStartTime
                  (ret', _, _) <- timed runTreeRecordTime (getBaseContext ctx) (runTreeLabel <> " (teardown)") $
                    runExampleM (runNodeCleanup intro) ctx runTreeLogs (Just [i|Failure in introduce '#{runTreeLabel}' cleanup handler|])
                  writeIORef result (ret', ExtraTimingInfo (Just setupFinishTime) (Just teardownStartTime))
            )
            (\(ret, _setupStartTime, setupFinishTime) -> do
                addSetupFinishTimeToStatus runTreeStatus setupFinishTime
                case ret of
                  Left failureReason@(Pending {}) -> do
                    -- TODO: add note about failure in allocation
                    markAllChildrenWithResult runNodeChildrenAugmented ctx (Failure failureReason)
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
  didRunWrappedAction <- liftIO $ newIORef (Left (), emptyExtraTimingInfo)
  runInAsync node ctx $ do
    let wrappedAction = do
          didAllocateVar <- liftIO $ newIORef False
          let handler e = do
                recordExceptionInStatus runTreeStatus e

                liftIO (readIORef didAllocateVar) >>= \case
                  False -> do
                    -- We didn't successfully allocate, so mark the children below this point as failed due to a
                    -- context issue.
                    markAllChildrenWithResult runNodeChildrenAugmented ctx $ case fromException e of
                      Just fr@(Pending {}) -> Failure fr
                      _ -> Failure $ GetContextException Nothing (SomeExceptionWithEq e)
                  True ->
                    -- Otherwise, let their existing statuses stand.
                    return ()

          flip withException handler $ do
            beginningCleanupVar <- liftIO $ newIORef Nothing

            -- Record a start event in the test timing, if configured
            -- TODO: how to do this? The speedscope viewer crashes if you don't properly nest events
            -- let tt = if runTreeRecordTime then getTestTimer (getBaseContext ctx) else NullTestTimer
            -- let setupLabel = runTreeLabel <> " (setup)"
            -- let teardownLabel = runTreeLabel <> " (teardown)"
            -- handleStartEvent tt (baseContextTestTimerProfile (getBaseContext ctx)) (T.pack setupLabel)

            results <- runNodeIntroduceAction $ \intro -> do
              -- Record the end event in the test timing
              -- TODO: do we need to deal with exceptions here and in teardown? I think we we fail to emit the
              -- end event, the time profile won't be viewable.
              -- handleEndEvent tt (baseContextTestTimerProfile (getBaseContext ctx)) (T.pack setupLabel)
              setupFinishTime <- liftIO getCurrentTime
              addSetupFinishTimeToStatus runTreeStatus setupFinishTime

              liftIO $ writeIORef didAllocateVar True

              (results, _, _) <- timed runTreeRecordTime (getBaseContext ctx) (runTreeLabel <> " (body)") $
                liftIO $ runNodesSequentially runNodeChildrenAugmented (LabelValue intro :> ctx)

              teardownStartTime <- liftIO getCurrentTime
              addTeardownStartTimeToStatus runTreeStatus teardownStartTime

              liftIO $ writeIORef beginningCleanupVar (Just teardownStartTime)
              -- handleStartEvent tt (baseContextTestTimerProfile (getBaseContext ctx)) (T.pack teardownLabel)
              liftIO $ writeIORef didRunWrappedAction (Right results, mkSetupTimingInfo setupFinishTime)
              return results

            liftIO (readIORef beginningCleanupVar) >>= \case
              Nothing -> return ()
              Just teardownStartTime -> do
                -- handleEndEvent tt (baseContextTestTimerProfile (getBaseContext ctx)) (T.pack teardownLabel)
                liftIO $ modifyIORef' didRunWrappedAction $ \(ret, timingInfo) ->
                  (ret, timingInfo { teardownStartTime = Just teardownStartTime })

            return results

          liftIO (readIORef didRunWrappedAction) >>= \case
            (Left (), timingInfo) -> return (Failure $ Reason Nothing [i|introduceWith '#{runTreeLabel}' handler didn't call action|], timingInfo)
            (Right _, timingInfo) -> return (Success, timingInfo)
    runExampleM' wrappedAction ctx runTreeLogs (Just [i|Exception in introduceWith '#{runTreeLabel}' handler|]) >>= \case
      Left err -> return (Failure err, emptyExtraTimingInfo)
      Right x -> pure x
startTree node@(RunNodeAround {..}) ctx' = do
  let RunNodeCommonWithStatus {..} = runNodeCommon
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  didRunWrappedAction <- liftIO $ newIORef (Left (), emptyExtraTimingInfo)
  runInAsync node ctx $ do
    let wrappedAction = do
          let failureResult e = case fromException e of
                Just fr@(Pending {}) -> Failure fr
                _ -> Failure $ Reason Nothing [i|around '#{runTreeLabel}' handler threw exception|]
          flip withException (\e -> recordExceptionInStatus runTreeStatus e) $ do
            runNodeActionWith $ do
              setupFinishTime <- liftIO getCurrentTime
              addSetupFinishTimeToStatus runTreeStatus setupFinishTime
              results <- liftIO $ runNodesSequentially runNodeChildren ctx
              liftIO $ writeIORef didRunWrappedAction (Right results, mkSetupTimingInfo setupFinishTime)
              return results

          (liftIO $ readIORef didRunWrappedAction) >>= \case
            (Left (), timingInfo) -> return (Failure $ Reason Nothing [i|around '#{runTreeLabel}' handler didn't call action|], timingInfo)
            (Right _, timingInfo) -> return (Success, timingInfo)
    runExampleM' wrappedAction ctx runTreeLogs (Just [i|Exception in around '#{runTreeLabel}' handler|]) >>= \case
      Left err -> return (Failure err, emptyExtraTimingInfo)
      Right x -> pure x
startTree node@(RunNodeDescribe {..}) ctx' = do
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  runInAsync node ctx $ do
    ((L.length . L.filter isFailure) <$> runNodesSequentially runNodeChildren ctx) >>= \case
      0 -> return (Success, emptyExtraTimingInfo)
      n -> return (Failure (ChildrenFailed Nothing n), emptyExtraTimingInfo)
startTree node@(RunNodeParallel {..}) ctx' = do
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  runInAsync node ctx $ do
    ((L.length . L.filter isFailure) <$> runNodesConcurrently runNodeCommon runNodeChildren ctx) >>= \case
      0 -> return (Success, emptyExtraTimingInfo)
      n -> return (Failure (ChildrenFailed Nothing n), emptyExtraTimingInfo)
startTree node@(RunNodeIt {..}) ctx' = do
  let ctx = modifyBaseContext ctx' $ baseContextFromCommon runNodeCommon
  runInAsync node ctx $ do
    (, emptyExtraTimingInfo) <$> runExampleM runNodeExample ctx (runTreeLogs runNodeCommon) Nothing

-- * Util

runInAsync :: (HasBaseContext context, MonadIO m) => RunNode context -> context -> IO (Result, ExtraTimingInfo) -> m (Async Result)
runInAsync node ctx action = do
  let RunNodeCommonWithStatus {..} = runNodeCommon node
  let bc@(BaseContext {..}) = getBaseContext ctx
  let timerFn = if runTreeRecordTime then timeAction' (getTestTimer bc) baseContextTestTimerProfile (T.pack runTreeLabel) else id
  startTime <- liftIO getCurrentTime
  mvar <- liftIO newEmptyMVar
  myAsync <- liftIO $ asyncWithUnmask $ \unmask -> do
    flip withException (recordExceptionInStatus runTreeStatus) $ unmask $ do
      readMVar mvar
      (result, extraTimingInfo) <- timerFn action
      endTime <- liftIO getCurrentTime
      liftIO $ atomically $ writeTVar runTreeStatus $ Done startTime (setupFinishTime extraTimingInfo) (teardownStartTime extraTimingInfo) endTime result

      whenFailure result $ \reason -> do
        -- Make sure the folder exists, if configured
        whenJust baseContextPath $ createDirectoryIfMissing True

        -- Create error symlink when configured to
        case node of
          RunNodeDescribe {} -> return () -- These are just noisy so don't create them
          RunNodeParallel {} -> return () -- These are just noisy so don't create them
          _ -> do
            whenJust baseContextErrorSymlinksDir $ \errorsDir ->
              whenJust baseContextPath $ \dir -> do
                whenJust baseContextRunRoot $ \runRoot -> do
                  let symlinkBaseName = case runTreeLoc of
                        Nothing -> takeFileName dir
                        Just loc -> [i|#{srcLocFile loc}_line#{srcLocStartLine loc}_#{takeFileName dir}|]
                  let symlinkPath = errorsDir </> (nodeToFolderName symlinkBaseName 9999999 runTreeId)

                  -- Delete the symlink if it's already present. This can happen when re-running
                  -- a previously failed test
                  exists <- doesPathExist symlinkPath
                  when exists $ removePathForcibly symlinkPath

#ifndef mingw32_HOST_OS
                  -- Get a relative path from the error dir to the results dir. System.FilePath doesn't want to
                  -- introduce ".." components, so we have to do it ourselves
                  let errorDirDepth = L.length $ splitPath $ makeRelative runRoot errorsDir

                  -- Don't do createDirectoryLink on Windows, as creating symlinks is generally not allowed for users.
                  -- See https://security.stackexchange.com/questions/10194/why-do-you-have-to-be-an-admin-to-create-a-symlink-in-windows
                  -- TODO: could we detect if this permission is available?
                  let relativePath = joinPath (L.replicate errorDirDepth "..") </> (makeRelative runRoot dir)
                  liftIO $ createDirectoryLink relativePath symlinkPath
#endif

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
  liftIO $ atomically $ writeTVar runTreeStatus $ Running startTime Nothing Nothing myAsync
  liftIO $ putMVar mvar ()
  return myAsync  -- TODO: fix race condition with writing to runTreeStatus (here and above)

-- | Run a list of children sequentially, cancelling everything on async exception
runNodesSequentially :: HasBaseContext context => [RunNode context] -> context -> IO [Result]
runNodesSequentially children ctx =
  flip withException (\(e :: SomeAsyncException) -> cancelAllChildrenWith children e) $
    forM (L.filter (shouldRunChild ctx) children) $ \child ->
      startTree child ctx >>= wait

-- | Run a list of children concurrently, cancelling everything on async exception
runNodesConcurrently :: forall context. HasBaseContext context => RunNodeCommon -> [RunNode context] -> context -> IO [Result]
runNodesConcurrently (RunNodeCommonWithStatus {runTreeLabel, runTreeId}) children ctx =
  flip withException (\(e :: SomeAsyncException) -> cancelAllChildrenWith children e) $
    mapM wait =<< sequence [startTree child (modifyTimingProfile ix ctx)
                           | (child, ix) <- L.zip runnableChildren [0..]]
  where
    runnableChildren = L.filter (shouldRunChild ctx) children

    leftPadWithZeros :: Int -> String
    leftPadWithZeros num = L.replicate (L.length (show (L.length runnableChildren)) - L.length (show num)) '0' <> show num

    modifyTimingProfile :: Int -> context -> context
    modifyTimingProfile n = flip modifyBaseContext (modifyTimingProfile' n)

    modifyTimingProfile' :: Int -> BaseContext -> BaseContext
    modifyTimingProfile' n bc@(BaseContext {..}) = bc {
      baseContextTestTimerProfile = baseContextTestTimerProfile <> [i|-#{runTreeLabel}-#{runTreeId}-#{leftPadWithZeros n}|]
      }

markAllChildrenWithResult :: (MonadIO m, HasBaseContext context') => [RunNode context] -> context' -> Result -> m ()
markAllChildrenWithResult children baseContext status = do
  now <- liftIO getCurrentTime
  forM_ (L.filter (shouldRunChild' baseContext) $ concatMap getCommons children) $ \child ->
    liftIO $ atomically $ modifyTVar (runTreeStatus child) $ \case
      Running {..} -> Done now statusSetupFinishTime statusTeardownStartTime now status
      done@(Done {}) -> done { statusResult = status }
      _ -> Done now Nothing Nothing now status

cancelAllChildrenWith :: [RunNode context] -> SomeAsyncException -> IO ()
cancelAllChildrenWith children e = do
  forM_ children $ \node ->
    readTVarIO (runTreeStatus $ runNodeCommon node) >>= \case
      Running {..} -> cancelWith statusAsync e
      NotStarted -> do
        now <- getCurrentTime
        let reason = GotAsyncException Nothing Nothing (SomeAsyncExceptionWithEq e)
        atomically $ writeTVar (runTreeStatus $ runNodeCommon node) (Done now Nothing Nothing now (Failure reason))
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

runExampleM' :: HasBaseContext r => ExampleM r a -> r -> TVar (Seq LogEntry) -> Maybe String -> IO (Either FailureReason a)
runExampleM' ex ctx logs exceptionMessage = do
  maybeTestDirectory <- getTestDirectory ctx
  let options = baseContextOptions $ getBaseContext ctx

  -- We want our handleAny call to be *inside* the withLogFn call, because
  -- withFile will catch IOException and fill in its own information, making the
  -- resulting error confusing
  withLogFn maybeTestDirectory options $ \logFn ->
    handleAny (wrapInFailureReasonIfNecessary exceptionMessage)
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
        Just (SomeExceptionWithCallStack e' cs) -> GotException (Just cs) msg (SomeExceptionWithEq (SomeException e'))
        _ -> GotException Nothing msg (SomeExceptionWithEq e)

addSetupFinishTimeToStatus :: (MonadIO m) => TVar Status -> UTCTime -> m ()
addSetupFinishTimeToStatus statusVar setupFinishTime = atomically $ modifyTVar statusVar $ \case
  status@(Running {}) -> status { statusSetupFinishTime = Just setupFinishTime }
  status@(Done {}) -> status { statusSetupFinishTime = Just setupFinishTime }
  x -> x

addTeardownStartTimeToStatus :: (MonadIO m) => TVar Status -> UTCTime -> m ()
addTeardownStartTimeToStatus statusVar t = atomically $ modifyTVar statusVar $ \case
  status@(Running {}) -> status { statusTeardownStartTime = Just t }
  status@(Done {}) -> status { statusTeardownStartTime = Just t }
  x -> x

recordExceptionInStatus :: (MonadIO m) => TVar Status -> SomeException -> m ()
recordExceptionInStatus status e = do
  endTime <- liftIO getCurrentTime
  let ret = case fromException e of
        Just (e' :: SomeAsyncException) -> Failure (GotAsyncException Nothing Nothing (SomeAsyncExceptionWithEq e'))
        _ -> case fromException e of
          Just (e' :: FailureReason) -> Failure e'
          _ -> Failure (GotException Nothing Nothing (SomeExceptionWithEq e))
  liftIO $ atomically $ modifyTVar status $ \case
    Running {..} -> Done statusStartTime statusSetupFinishTime statusTeardownStartTime endTime ret
    _ -> Done endTime Nothing Nothing endTime ret

timed :: (MonadUnliftIO m) => Bool -> BaseContext -> String -> m a -> m (a, UTCTime, UTCTime)
timed recordTime bc@(BaseContext {..}) label action = do
  let timerFn = if recordTime then timeAction' (getTestTimer bc) baseContextTestTimerProfile (T.pack label) else id

  startTime <- liftIO getCurrentTime
  ret <- timerFn action
  endTime <- liftIO getCurrentTime
  pure (ret, startTime, endTime)
