{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

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
import System.Directory
import System.FilePath
import System.IO
import Test.Sandwich.Types.TestTimer


-- * User functions

defaultProfileName :: T.Text
defaultProfileName = "default"

timeActionByProfile :: (MonadMask m, MonadIO m, MonadReader context m, HasTestTimer context) => T.Text -> T.Text -> m a -> m a
timeActionByProfile profileName eventName action = do
  tt <- asks getTestTimer
  timeAction' tt profileName eventName action

timeAction :: (MonadMask m, MonadIO m, MonadReader context m, HasTestTimer context) => T.Text -> m a -> m a
timeAction eventName action = do
  tt <- asks getTestTimer
  timeAction' tt defaultProfileName eventName action

-- * Core

newSpeedScopeTestTimer :: FilePath -> IO TestTimer
newSpeedScopeTestTimer path = do
  createDirectoryIfMissing True path
  h <- openFile (path </> "timings_raw.txt") AppendMode
  hSetBuffering h LineBuffering
  speedScopeFile <- newMVar emptySpeedScopeFile
  return $ TestTimer path h speedScopeFile

finalizeSpeedScopeTestTimer :: TestTimer -> IO ()
finalizeSpeedScopeTestTimer NullTestTimer = return ()
finalizeSpeedScopeTestTimer (TestTimer {..}) = do
  hClose testTimerHandle
  readMVar testTimerSpeedScopeFile >>= BL.writeFile (testTimerBasePath </> "speedscope.json") . A.encode

timeAction' :: (MonadMask m, MonadIO m) => TestTimer -> T.Text -> T.Text -> m a -> m a
timeAction' NullTestTimer _ _ = id
timeAction' (TestTimer {..}) profileName eventName = bracket_
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
      T.hPutStrLn testTimerHandle [i|#{time} START #{show profileName} #{eventName}|]
      return $ handleSpeedScopeEvent file time SpeedScopeEventTypeOpen

    handleEndEvent file time = do
      T.hPutStrLn testTimerHandle [i|#{time} END #{show profileName} #{eventName}|]
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
