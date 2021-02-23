{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
module Test.Sandwich.TestTimer where

import Control.Concurrent
import Control.Exception.Safe
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Trans.State
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import qualified Data.Sequence as S
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX
import GHC.Stack
import System.Directory
import System.FilePath
import System.IO
import Test.Sandwich.Contexts
import Test.Sandwich.Nodes
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Test.Sandwich.Types.TestTimer


-- * Introduction machinery

testTimerLabel = Label :: Label "testTimer" TestTimer
type HasTestTimer context = HasLabel context "testTimer" TestTimer

testTimerProfile = Label :: Label "testTimerProfile" T.Text
type HasTestTimerProfile context = (HasTestTimer context, HasLabel context "testTimerProfile" T.Text)

introduceTestTimer :: (HasBaseContext context, MonadIO m, HasCallStack) => SpecFree (LabelValue "testTimer" TestTimer :> context) m () -> SpecFree context m ()
introduceTestTimer = introduce' (defaultNodeOptions {nodeOptionsCreateFolder=False}) "Test timer" testTimerLabel alloc (liftIO . finalizeTestTimer) where
  alloc = do
    getRunRoot >>= \case
      Nothing -> error "Test timer context requires a run root"
      Just p -> liftIO $ newTestTimer p

-- * Simple timing node

timingNodeByProfile :: (MonadIO m, MonadMask m, HasTestTimer context, HasTestTimerProfile context) => String -> SpecFree context m () -> SpecFree context m ()
timingNodeByProfile name = around ("Timer for " <> name) (void . testTimerByProfile (T.pack name))

-- * Core

withTestTimer :: (MonadMask m, MonadIO m) => FilePath -> (TestTimer -> m a) -> m a
withTestTimer path action = bracket (liftIO $ newTestTimer path) (liftIO . finalizeTestTimer) action

newTestTimer :: FilePath -> IO TestTimer
newTestTimer path = do
  createDirectoryIfMissing True path
  h <- openFile (path </> "timings_raw.txt") AppendMode
  hSetBuffering h LineBuffering
  speedScopeFile <- newMVar emptySpeedScopeFile
  return $ TestTimer path h speedScopeFile

finalizeTestTimer :: TestTimer -> IO ()
finalizeTestTimer NullTestTimer = return ()
finalizeTestTimer (TestTimer {..}) = do
  hClose testTimerHandle

  file <- readMVar testTimerSpeedScopeFile
  BL.writeFile (testTimerBasePath </> "speedscope.json") (A.encode file)

testTimerByProfile :: (MonadMask m, MonadIO m, MonadReader context m, HasTestTimerProfile context) => T.Text -> m a -> m a
testTimerByProfile name action = do
  tt <- getContext testTimerLabel
  profileName <- getContext testTimerProfile
  testTimer tt profileName name action

testTimer :: (MonadMask m, MonadIO m) => TestTimer -> T.Text -> T.Text -> m a -> m a
testTimer tt profileName name action = bracket
  (liftIO $ do
      modifyMVar_ (testTimerSpeedScopeFile tt) $ \file -> do
        now <- getPOSIXTime
        handleStartEvent tt file profileName name now
  )
  (\_ -> liftIO $ do
      modifyMVar_ (testTimerSpeedScopeFile tt) $ \file -> do
        now <- getPOSIXTime
        handleEndEvent tt file profileName name now
  )
  (\_ -> action)

handleStartEvent NullTestTimer file _ _ _ = return file
handleStartEvent tt@(TestTimer {..}) file profileName name time = do
  T.hPutStrLn testTimerHandle [i|#{time} START #{show profileName} #{name}|]
  return $ handleSpeedScopeEvent tt file profileName name time SpeedScopeEventTypeOpen

handleEndEvent NullTestTimer file _ _ _ = return file
handleEndEvent tt@(TestTimer {..}) file profileName name time = do
  T.hPutStrLn testTimerHandle [i|#{time} END #{show profileName} #{name}|]
  return $ handleSpeedScopeEvent tt file profileName name time SpeedScopeEventTypeClose

-- | TODO: maybe use an intermediate format so the frames (and possibly profiles) aren't stored as lists,
-- so we don't have to do O(N) L.length and S.findIndexL
handleSpeedScopeEvent :: TestTimer -> SpeedScopeFile -> T.Text -> T.Text -> POSIXTime -> SpeedScopeEventType -> SpeedScopeFile
handleSpeedScopeEvent NullTestTimer initialFile _ _ _ _ = initialFile
handleSpeedScopeEvent (TestTimer {..}) initialFile profileName eventName time typ = flip execState initialFile $ do
  frameID <- get >>= \f -> case S.findIndexL (== SpeedScopeFrame eventName) (f ^. shared . frames) of
    Just i -> return i
    Nothing -> do
      modify' $ over (shared . frames) (S.|> (SpeedScopeFrame eventName))
      return $ S.length $ f ^. shared . frames

  profileIndex <- get >>= \f -> case L.findIndex ((== profileName) . (^. name)) (f ^. profiles) of
    Just i -> return i
    Nothing -> do
      modify' $ over profiles (\x -> x <> [newProfile profileName time])
      return $ L.length (f ^. profiles)

  modify' $ \f -> f
    & over (profiles . ix profileIndex . events) (S.|> (SpeedScopeEvent typ frameID time))
    & over (profiles . ix profileIndex . endValue) (max time)
