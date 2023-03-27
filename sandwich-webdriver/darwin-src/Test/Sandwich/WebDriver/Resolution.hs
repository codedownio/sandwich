
module Test.Sandwich.WebDriver.Resolution (
  getResolution
  , getResolutionForDisplay
  ) where

import Data.Word


getResolution :: IO (Int, Int, Int, Int)
getResolution = do
  ddid <- cg_main_display_id
  getResolutionForDisplay (fromIntegral ddid)

-- TODO: currently we just use the "main display" ID.
-- When when we return 0 for x and y here, the WebDriver setWindowPos
-- should just position the window on the main display.
-- This probably won't work at all for xvfb displays, but we have more work to do there...
getResolutionForDisplay :: Int -> IO (Int, Int, Int, Int)
getResolutionForDisplay ddid = do
  width <- cg_display_pixels_wide (fromIntegral ddid)
  height <- cg_display_pixels_high (fromIntegral ddid)
  return (0, 0, fromIntegral width, fromIntegral height)

type CGDirectDisplayID = Word32

foreign import ccall unsafe "<CoreGraphics/CGDisplayConfiguration.h> CGMainDisplayID"
  cg_main_display_id :: IO CGDirectDisplayID

foreign import ccall unsafe "<CoreGraphics/CGDisplayConfiguration.h> CGDisplayPixelsWide"
  cg_display_pixels_wide :: CGDirectDisplayID -> IO CGDirectDisplayID

foreign import ccall unsafe "<CoreGraphics/CGDisplayConfiguration.h> CGDisplayPixelsHigh"
  cg_display_pixels_high :: CGDirectDisplayID -> IO CGDirectDisplayID
