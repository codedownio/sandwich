{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}

module Test.Sandwich.TestTimer where

import Control.Concurrent
import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.State
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Sequence as S
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX
import Lens.Micro
import System.Directory
import System.FilePath
import System.IO
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Test.Sandwich.Types.TestTimer
import Test.Sandwich.Util (whenJust)


type EventName = T.Text
type ProfileName = T.Text

-- * User functions

-- | Time a given action with a given event name. This name will be the "stack frame" of the given action in the profiling results. This function will use the current timing profile name.
timeAction :: (MonadMask m, MonadIO m, MonadReader context m, HasBaseContext context, HasTestTimer context) => EventName -> m a -> m a
timeAction eventName action = do
  tt <- asks getTestTimer
  BaseContext {baseContextTestTimerProfile} <- asks getBaseContext
  timeAction' tt baseContextTestTimerProfile eventName action

-- | Time a given action with a given profile name and event name. Use when you want to manually specify the profile name.
timeActionByProfile :: (MonadMask m, MonadIO m, MonadReader context m, HasTestTimer context) => ProfileName -> EventName -> m a -> m a
timeActionByProfile profileName eventName action = do
  tt <- asks getTestTimer
  timeAction' tt profileName eventName action

-- | Introduce a new timing profile name.
withTimingProfile :: (Monad m) => ProfileName -> SpecFree (LabelValue "testTimerProfile" TestTimerProfile :> context) m () -> SpecFree context m ()
withTimingProfile name = introduce' timingNodeOptions [i|Switch test timer profile to '#{name}'|] testTimerProfile (pure $ TestTimerProfile name) (\_ -> return ())

-- | Introduce a new timing profile name dynamically. The given 'ExampleT' should come up with the name and return it.
withTimingProfile' :: (Monad m) => ExampleT context m ProfileName -> SpecFree (LabelValue "testTimerProfile" TestTimerProfile :> context) m () -> SpecFree context m ()
withTimingProfile' getName = introduce' timingNodeOptions [i|Switch test timer profile to dynamic value|] testTimerProfile (TestTimerProfile <$> getName) (\_ -> return ())

-- * Core

timingNodeOptions :: NodeOptions
timingNodeOptions = defaultNodeOptions { nodeOptionsRecordTime = False
                                       , nodeOptionsCreateFolder = False
                                       , nodeOptionsVisibilityThreshold = systemVisibilityThreshold }

newSpeedScopeTestTimer :: FilePath -> Bool -> IO TestTimer
newSpeedScopeTestTimer path writeRawTimings = do
  createDirectoryIfMissing True path

  maybeHandle <- case writeRawTimings of
    False -> return Nothing
    True -> do
      h <- openFile (path </> "timings_raw.txt") AppendMode
      hSetBuffering h LineBuffering
      return $ Just h

  speedScopeFile <- newMVar emptySpeedScopeFile
  return $ SpeedScopeTestTimer path maybeHandle speedScopeFile

finalizeSpeedScopeTestTimer :: TestTimer -> IO ()
finalizeSpeedScopeTestTimer NullTestTimer = return ()
finalizeSpeedScopeTestTimer (SpeedScopeTestTimer {..}) = do
  whenJust testTimerHandle hClose
  readMVar testTimerSpeedScopeFile >>= BL.writeFile (testTimerBasePath </> "speedscope.json") . A.encode

timeAction' :: (MonadMask m, MonadIO m) => TestTimer -> T.Text -> T.Text -> m a -> m a
timeAction' NullTestTimer _ _ = id
timeAction' (SpeedScopeTestTimer {..}) profileName eventName = bracket_
  (liftIO $ modifyMVar_ testTimerSpeedScopeFile $ \file -> do
    now <- getPOSIXTime
    handleStartEvent file now
  )
  (liftIO $ modifyMVar_ testTimerSpeedScopeFile $ \file -> do
    now <- getPOSIXTime
    handleEndEvent file now
  )
  where
    handleStartEvent file time = do
      whenJust testTimerHandle $ \h -> T.hPutStrLn h [i|#{time} START #{show profileName} #{eventName}|]
      return $ handleSpeedScopeEvent file time SpeedScopeEventTypeOpen

    handleEndEvent file time = do
      whenJust testTimerHandle $ \h -> T.hPutStrLn h [i|#{time} END #{show profileName} #{eventName}|]
      return $ handleSpeedScopeEvent file time SpeedScopeEventTypeClose

    -- | TODO: maybe use an intermediate format so the frames (and possibly profiles) aren't stored as lists,
    -- so we don't have to do O(N) L.length and S.findIndexL
    handleSpeedScopeEvent :: SpeedScopeFile -> POSIXTime -> SpeedScopeEventType -> SpeedScopeFile
    handleSpeedScopeEvent initialFile time typ = flip execState initialFile $ do
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

      modify' $ over (profiles . ix profileIndex . events) (S.|> (SpeedScopeEvent typ frameID time))
              . over (profiles . ix profileIndex . endValue) (max time)
