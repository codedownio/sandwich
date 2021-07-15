{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Sandwich.WebDriver.Resolution (
  getResolution
  , getResolutionForDisplay
  ) where

import Control.Exception
import Data.Function
import qualified Data.List as L
import Data.String.Interpolate
import qualified Data.Text as T
import Safe
import System.Directory
import System.Exit
import System.Process
import Text.Regex


-- | Previously we got the screen resolution on Linux using the X11 Haskell library.
-- This was a troublesome dependency because it wouldn't build on Hackage, forcing us to upload
-- sandwich-webdriver documentation manually.
-- It also caused problems when trying to make the demos easy to run on a clean machine or a Mac.
-- Instead, we implement platform-specific getResolution functions.
-- On Linux, the simplest way seems to be to parse the output of xrandr. This is the approach taken by
-- at least one other library: https://github.com/davidmarkclements/screenres/blob/master/linux.cc
-- The other way to do it would be to load the x11 and/or xinerama libraries like is done here:
-- https://github.com/rr-/screeninfo/blob/master/screeninfo/enumerators/xinerama.py
-- but again, that would require users to install those libraries. xrandr itself seems like an easier
-- dependency.
getResolution :: IO (Int, Int, Int, Int)
getResolution = getResolution' Nothing

getResolutionForDisplay :: Int -> IO (Int, Int, Int, Int)
getResolutionForDisplay n = getResolution' (Just [("DISPLAY", ":" <> show n)])

getResolution' :: Maybe [(String, String)] -> IO (Int, Int, Int, Int)
getResolution' xrandrEnv = do
  xrandrPath <- findExecutable "xrandr" >>= \case
    Just x -> return x
    Nothing -> throwIO $ userError "Couldn't find xrandr executable. Please make sure it's in the path so that sandwich can get the screen resolution."

  (exitCode, sout, serr) <- readCreateProcessWithExitCode ((proc xrandrPath []) { env = xrandrEnv }) ""
  case exitCode of
    ExitSuccess -> return ()
    ExitFailure n -> throwIO $ userError [i|Couldn't parse xrandr output to find screen resolution (exit code #{n}).\n***Stdout***\n\n#{sout}\n\n***Stderr***\n\n#{serr}|]

  let connectedLines = sout
                     & T.lines . T.pack
                     & filter ("connected" `T.isInfixOf`)
                     & L.sortBy preferPrimary

  case headMay [(x, y, w, h) | (matchRegex resolutionRegex -> Just [(readMay -> Just w), (readMay -> Just h), (readMay -> Just x), (readMay -> Just y)]) <- fmap T.unpack connectedLines] of
    Nothing -> throwIO $ userError "Couldn't parse xrandr output to find screen resolution.\n\n***Stdout***\n\n#{stdout}"
    Just x -> return x

resolutionRegex :: Regex
resolutionRegex = mkRegex "([0-9]+)x([0-9]+)\\+([0-9]+)\\+([0-9]+)"

preferPrimary :: T.Text -> T.Text -> Ordering
preferPrimary x y =
  if | xPrimary && yPrimary -> EQ
     | xPrimary -> LT
     | yPrimary -> GT
     | otherwise -> EQ
  where
    xPrimary = "primary" `L.elem` (T.words x)
    yPrimary = "primary" `L.elem` (T.words y)
