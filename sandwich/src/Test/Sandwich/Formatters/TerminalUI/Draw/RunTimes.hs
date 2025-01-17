{-# LANGUAGE MultiWayIf #-}

module Test.Sandwich.Formatters.TerminalUI.Draw.RunTimes (
  getRunTimes
  ) where

import Brick
import Data.Maybe
import Data.String.Interpolate
import Data.Time.Clock
import qualified Graphics.Vty as V
import Lens.Micro
import Test.Sandwich.Formatters.Common.Util
import Test.Sandwich.Formatters.TerminalUI.AttrMap
import Test.Sandwich.Formatters.TerminalUI.Types


minGray, maxGray :: Int
minGray = 50
maxGray = 255

data Mode =
  NothingRunning
  | SetupRunning
  | WorkRunning
  | TeardownRunning
  deriving (Eq)

getRunTimes :: AppState -> UTCTime -> Maybe UTCTime -> Maybe UTCTime -> UTCTime -> Bool -> Widget n
getRunTimes app startTime statusSetupFinishTime statusTeardownStartTime endTime showEllipses = raw setupWork <+> raw actualWork <+> raw teardownWork
  where
    totalElapsed = diffUTCTime (app ^. appCurrentTime) (app ^. appStartTime)

    setupTime = diffUTCTime <$> statusSetupFinishTime <*> pure startTime
    teardownTime = diffUTCTime <$> pure endTime <*> statusTeardownStartTime

    actualWorkTime = (diffUTCTime endTime startTime) - (fromMaybe 0 setupTime) - (fromMaybe 0 teardownTime)

    mode = if
      | not showEllipses -> NothingRunning
      | isJust statusTeardownStartTime -> TeardownRunning
      | isJust statusSetupFinishTime -> WorkRunning
      | otherwise -> SetupRunning

    setupWork = maybe mempty (\dt -> V.string (getAttr totalElapsed dt (mode == SetupRunning)) [i|(#{formatNominalDiffTime dt}) + |]) setupTime
    actualWork = V.string (getAttr totalElapsed actualWorkTime (mode == WorkRunning)) (formatNominalDiffTime actualWorkTime)
    teardownWork = maybe mempty (\dt -> V.string (getAttr totalElapsed dt (mode == TeardownRunning)) [i| + (#{formatNominalDiffTime dt})|]) teardownTime

getAttr :: NominalDiffTime -> NominalDiffTime -> Bool -> V.Attr
getAttr totalElapsed dt bold = V.Attr {
  V.attrStyle = if bold then (V.SetTo V.bold) else V.Default
  , V.attrForeColor = V.SetTo $ grayAt $ getLevel (realToFrac totalElapsed) (realToFrac dt)
  , V.attrBackColor = V.Default
  , V.attrURL = V.Default
  }

getLevel :: Double -> Double -> Int
getLevel totalElapsed duration = min maxGray $ max minGray $ round (fromIntegral minGray + (intensity * (fromIntegral (maxGray - minGray))))
  where
    intensity :: Double = logBase (totalElapsed + 1) (duration + 1)
