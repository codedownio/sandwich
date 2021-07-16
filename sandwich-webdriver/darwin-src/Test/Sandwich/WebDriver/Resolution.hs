{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ViewPatterns #-}

module Test.Sandwich.WebDriver.Resolution (
  getResolution
  , getResolutionForDisplay
  ) where

import Data.Word


getResolution :: IO (Int, Int, Int, Int)
getResolution = do
  ddid <- cg_main_display_id
  width <- cg_display_pixels_wide ddid
  height <- cg_display_pixels_high ddid
  return (0, 0, fromIntegral width, fromIntegral height)

getResolutionForDisplay :: Int -> IO (Int, Int, Int, Int)
getResolutionForDisplay = undefined

type CGDirectDisplayID = Word32

foreign import ccall unsafe "<CoreGraphics/CGDisplayConfiguration.h> CGMainDisplayID"
  cg_main_display_id :: IO CGDirectDisplayID

foreign import ccall unsafe "<CoreGraphics/CGDisplayConfiguration.h> CGDisplayPixelsWide"
  cg_display_pixels_wide :: CGDirectDisplayID -> IO CGDirectDisplayID

foreign import ccall unsafe "<CoreGraphics/CGDisplayConfiguration.h> CGDisplayPixelsHigh"
  cg_display_pixels_high :: CGDirectDisplayID -> IO CGDirectDisplayID
