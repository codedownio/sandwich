
module Test.Sandwich.Formatters.TerminalUI.Draw.RunTimes (getRunTimes) where

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

getRunTimes :: AppState -> UTCTime -> UTCTime -> Maybe NominalDiffTime -> Maybe NominalDiffTime -> Bool -> Widget n
getRunTimes app startTime endTime statusSetupTime statusTeardownTime showEllipses = raw setupWork <+> raw actualWork <+> raw teardownWork
  where
    totalElapsed = diffUTCTime (app ^. appCurrentTime) (app ^. appStartTime)

    actualWorkTime = (diffUTCTime endTime startTime) - (fromMaybe 0 statusSetupTime) - (fromMaybe 0 statusTeardownTime)

    setupWork = maybe mempty (\dt -> V.string (getAttr totalElapsed dt) [i|(#{formatNominalDiffTime dt}) + |]) statusSetupTime
    actualWork = V.string (getAttr totalElapsed actualWorkTime) (formatNominalDiffTime actualWorkTime <> (if showEllipses then "..." else ""))
    teardownWork = maybe mempty (\dt -> V.string (getAttr totalElapsed dt) [i| + (#{formatNominalDiffTime dt})|]) statusTeardownTime

getAttr :: NominalDiffTime -> NominalDiffTime -> V.Attr
getAttr totalElapsed dt = V.Attr {
  V.attrStyle = V.Default
  , V.attrForeColor = V.SetTo $ grayAt $ getLevel (realToFrac totalElapsed) (realToFrac dt)
  , V.attrBackColor = V.Default
  , V.attrURL = V.Default
  }

getLevel :: Double -> Double -> Int
getLevel totalElapsed duration = min maxGray $ max minGray $ round (fromIntegral minGray + (intensity * (fromIntegral (maxGray - minGray))))
  where
    intensity :: Double = logBase (totalElapsed + 1) (duration + 1)
