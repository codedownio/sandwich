{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.TestTimer (
  timeAction
  , timeAction'
  , timeActionByProfile

  , handleStartEvent
  , handleEndEvent

  , withTimingProfile
  , withTimingProfile'

  , newTimingLaneSource
  , withTimingLane
  , inTimingLane

  , newSpeedScopeTestTimer
  , finalizeSpeedScopeTestTimer
  , renderSpeedScopeFile
  ) where

import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import Control.Monad.Trans.State
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Sequence as S
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time
import Data.Time.Clock.POSIX
import Data.Unique
import Lens.Micro
import System.Directory
import System.FilePath
import System.IO
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Test.Sandwich.Types.TestTimer
import Test.Sandwich.Util (whenJust)
import UnliftIO.STM
import UnliftIO.Concurrent
import UnliftIO.Exception


type EventName = T.Text
type ProfileName = T.Text

allTestsEventName :: EventName
allTestsEventName = "All tests"

-- * User functions

-- | Time a given action with a given event name. This name will be the "stack
-- frame" of the given action in the profiling results. This function will use
-- the current timing profile name.
timeAction :: (MonadUnliftIO m, HasBaseContextMonad context m, HasTestTimer context)
  -- | Event name
  => EventName
  -> m a
  -> m a
timeAction eventName action = do
  tt <- asks getTestTimer
  bc <- asks getBaseContext
  profile <- currentTestTimerProfile bc
  timeAction' tt profile eventName action

-- | Time a given action with a given profile name and event name. Use when you
-- want to manually specify the profile name.
timeActionByProfile :: (MonadUnliftIO m, MonadReader context m, HasTestTimer context)
  -- | Profile name
  => ProfileName
  -- | Event name
  -> EventName
  -> m a
  -> m a
timeActionByProfile profileName eventName action = do
  tt <- asks getTestTimer
  timeAction' tt profileName eventName action

-- | Introduce a new timing profile name.
withTimingProfile :: (Monad m)
  -- | Profile name
  => ProfileName
  -> SpecFree (LabelValue "testTimerProfile" TestTimerProfile :> context) m ()
  -> SpecFree context m ()
withTimingProfile pn = introduce' timingNodeOptions [i|Switch test timer profile to '#{pn}'|] testTimerProfile (pure $ TestTimerProfile pn) (\_ -> return ())

-- | Introduce a new timing profile name dynamically. The given 'ExampleT'
-- should come up with the name and return it.
withTimingProfile' :: (Monad m)
  -- | Callback to generate the profile name
  => ExampleT context m ProfileName
  -> SpecFree (LabelValue "testTimerProfile" TestTimerProfile :> context) m ()
  -> SpecFree context m ()
withTimingProfile' getName = introduce' timingNodeOptions [i|Switch test timer profile to dynamic value|] testTimerProfile (TestTimerProfile <$> getName) (\_ -> return ())

-- * Timing lanes

-- | Make a new source of timing lanes. Anything claiming a lane with 'withTimingLane' should pass
-- the same source, so that nested claims from that source can be detected.
newTimingLaneSource :: MonadIO m => m TimingLaneSource
newTimingLaneSource = TimingLaneSource <$> liftIO newUnique

-- | Record this action, and everything below it in the test tree, under the given profile.
--
-- This is how something that hands out lanes (see 'Test.Sandwich.ParallelN.parallelN') gets
-- everything running in a lane to share a profile: nodes read the profile when they start, so
-- unlike 'withTimingProfile' this works from an 'around' handler, without changing the type of the
-- spec underneath.
--
-- Concurrent branches of the tree never share the profile set here; a 'Test.Sandwich.parallel'
-- node below gives each of its children a suffixed profile of its own, so their frames can't
-- interleave.
withTimingLane :: (MonadUnliftIO m, HasBaseContextMonad context m)
  -- | Who's handing out the lane
  => TimingLaneSource
  -- | Profile name for the lane
  -> ProfileName
  -> m a
  -> m a
withTimingLane source profileName action = do
  BaseContext {baseContextCurrentTimingLane} <- asks getBaseContext
  previous <- readTVarIO baseContextCurrentTimingLane
  let held = TimingLaneState (source : maybe [] timingLaneSources previous) profileName
  bracket_ (atomically $ writeTVar baseContextCurrentTimingLane (Just held))
           (atomically $ writeTVar baseContextCurrentTimingLane previous)
           action

-- | Whether this branch of the tree is already inside a lane from the given source. Claiming a
-- second lane from the same source would deadlock.
inTimingLane :: (MonadIO m, HasBaseContextMonad context m) => TimingLaneSource -> m Bool
inTimingLane source = do
  BaseContext {baseContextCurrentTimingLane} <- asks getBaseContext
  readTVarIO baseContextCurrentTimingLane >>= \case
    Just (TimingLaneState {timingLaneSources}) -> pure (source `elem` timingLaneSources)
    Nothing -> pure False

-- * Core

timingNodeOptions :: NodeOptions
timingNodeOptions = defaultNodeOptions {
  nodeOptionsRecordTime = False
  , nodeOptionsCreateFolder = False
  , nodeOptionsVisibilityThreshold = systemVisibilityThreshold
  }

newSpeedScopeTestTimer :: FilePath -> Bool -> IO TestTimer
newSpeedScopeTestTimer path writeRawTimings = do
  startTime <- liftIO getPOSIXTime

  createDirectoryIfMissing True path

  maybeHandle <- case writeRawTimings of
    False -> return Nothing
    True -> do
      h <- openFile (path </> "timings_raw.txt") AppendMode
      hSetBuffering h LineBuffering
      return $ Just h

  speedScopeFile <- newMVar emptySpeedScopeFile
  return $ SpeedScopeTestTimer startTime path maybeHandle speedScopeFile

finalizeSpeedScopeTestTimer :: TestTimer -> IO ()
finalizeSpeedScopeTestTimer NullTestTimer = return ()
finalizeSpeedScopeTestTimer tt@(SpeedScopeTestTimer {..}) = do
  contents <- renderSpeedScopeFile tt

  whenJust testTimerHandle hClose

  whenJust contents $ BL.writeFile (testTimerBasePath </> "speedscope.json")

-- | Render the current state of the test timer as a speedscope profile. Any
-- frames that are still open are closed off at the current time, so this can be
-- called while tests are running.
renderSpeedScopeFile :: MonadIO m => TestTimer -> m (Maybe BL.ByteString)
renderSpeedScopeFile NullTestTimer = return Nothing
renderSpeedScopeFile (SpeedScopeTestTimer {..}) = liftIO $ do
  endTime <- getPOSIXTime

  speedScopeFile <- readMVar testTimerSpeedScopeFile

  -- Wrap every test profile in an overall frame called 'allTestsEventName'. If
  -- we don't do this, the speedscope viewer will show each profile as if it
  -- starts at time 0.
  let finalSpeedScopeFile :: SpeedScopeFile = L.foldl'
        (\ssf profileName ->
           ssf
           & prependSpeedScopeEvent testTimerStartTime profileName allTestsEventName SpeedScopeEventTypeOpen
           & appendSpeedScopeEvent endTime profileName allTestsEventName SpeedScopeEventTypeClose
        )
        (closeOpenFrames endTime speedScopeFile)
        (fmap (^. name) (speedScopeFile ^. profiles))

  return $ Just $ A.encode finalSpeedScopeFile

  where
    closeOpenFrames :: POSIXTime -> SpeedScopeFile -> SpeedScopeFile
    closeOpenFrames time = over profiles (fmap closeProfile)
      where
        closeProfile :: SpeedScopeProfile -> SpeedScopeProfile
        closeProfile p = p
          & over events (<> S.fromList [SpeedScopeEvent SpeedScopeEventTypeClose frameID time
                                       | frameID <- openFrames (p ^. events)])
          & over endValue (max time)

        openFrames :: S.Seq SpeedScopeEvent -> [Int]
        openFrames = L.foldl' step []
          where
            step open (SpeedScopeEvent SpeedScopeEventTypeOpen frameID _) = frameID : open
            step (_:rest) (SpeedScopeEvent SpeedScopeEventTypeClose _ _) = rest
            step [] (SpeedScopeEvent SpeedScopeEventTypeClose _ _) = []

timeAction' :: (MonadUnliftIO m) => TestTimer -> T.Text -> T.Text -> m a -> m a
timeAction' NullTestTimer _ _ = id
timeAction' (SpeedScopeTestTimer {..}) profileName eventName = bracket_
  (modifyMVar_ testTimerSpeedScopeFile $ \file -> liftIO getPOSIXTime >>= handleStartEvent' testTimerHandle profileName eventName file)
  (modifyMVar_ testTimerSpeedScopeFile $ \file -> liftIO getPOSIXTime >>= handleEndEvent' testTimerHandle profileName eventName file)

handleStartEvent :: (MonadUnliftIO m) => TestTimer -> T.Text -> T.Text -> m ()
handleStartEvent NullTestTimer _profileName _eventName = return ()
handleStartEvent (SpeedScopeTestTimer {..}) profileName eventName =
  modifyMVar_ testTimerSpeedScopeFile $ \file ->
    liftIO getPOSIXTime >>= handleStartEvent' testTimerHandle profileName eventName file

handleStartEvent' :: (MonadIO m)
  => Maybe Handle
  -> T.Text
  -> T.Text
  -> SpeedScopeFile
  -> NominalDiffTime
  -> m SpeedScopeFile
handleStartEvent' maybeHandle profileName eventName file time = do
  whenJust maybeHandle $ \h -> liftIO $ T.hPutStrLn h [i|#{time} START #{show profileName} #{eventName}|]
  return $ appendSpeedScopeEvent time profileName eventName SpeedScopeEventTypeOpen file

handleEndEvent :: (MonadUnliftIO m) => TestTimer -> T.Text -> T.Text -> m ()
handleEndEvent NullTestTimer _profileName _eventName = return ()
handleEndEvent (SpeedScopeTestTimer {..}) profileName eventName =
  modifyMVar_ testTimerSpeedScopeFile $ \file ->
    liftIO getPOSIXTime >>= handleEndEvent' testTimerHandle profileName eventName file

handleEndEvent' :: (MonadIO m)
  => Maybe Handle
  -> T.Text
  -> T.Text
  -> SpeedScopeFile
  -> NominalDiffTime
  -> m SpeedScopeFile
handleEndEvent' maybeHandle profileName eventName file time = do
  whenJust maybeHandle $ \h -> liftIO $ T.hPutStrLn h [i|#{time} END #{show profileName} #{eventName}|]
  return $ appendSpeedScopeEvent time profileName eventName SpeedScopeEventTypeClose file

appendSpeedScopeEvent :: POSIXTime -> T.Text -> T.Text -> SpeedScopeEventType -> SpeedScopeFile -> SpeedScopeFile
appendSpeedScopeEvent time profileName eventName eventType initialFile = flip execState initialFile $ do
  (frameID, profileIndex) <- getFrameIDAndProfileIndex time profileName eventName

  modify' $ over (profiles . ix profileIndex . events) (S.|> (SpeedScopeEvent eventType frameID time))
          . over (profiles . ix profileIndex . endValue) (max time)

prependSpeedScopeEvent :: POSIXTime -> T.Text -> T.Text -> SpeedScopeEventType -> SpeedScopeFile -> SpeedScopeFile
prependSpeedScopeEvent time profileName eventName eventType initialFile = flip execState initialFile $ do
  (frameID, profileIndex) <- getFrameIDAndProfileIndex time profileName eventName

  modify' $ over (profiles . ix profileIndex . events) ((SpeedScopeEvent eventType frameID time) S.<|)
          . over (profiles . ix profileIndex . startValue) (min time)

-- | TODO: maybe use an intermediate format so the frames (and possibly
-- profiles) aren't stored as lists, so we don't have to do O(N) L.length and
-- S.findIndexL
getFrameIDAndProfileIndex :: POSIXTime -> T.Text -> T.Text -> State SpeedScopeFile (Int, Int)
getFrameIDAndProfileIndex time profileName eventName = do
  frameID <- get >>= \f -> case S.findIndexL (== SpeedScopeFrame eventName) (f ^. shared . frames) of
    Just j -> return j
    Nothing -> do
      modify' $ over (shared . frames) (S.|> (SpeedScopeFrame eventName))
      return $ S.length $ f ^. shared . frames

  profileIndex <- get >>= \f -> case L.findIndex ((== profileName) . (^. name)) (f ^. profiles) of
    Just j -> return j
    Nothing -> do
      modify' $ over profiles (\x -> x <> [newProfile profileName time])
      return $ L.length (f ^. profiles)

  return (frameID, profileIndex)
