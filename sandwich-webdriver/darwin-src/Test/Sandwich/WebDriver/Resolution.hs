
module Test.Sandwich.WebDriver.Resolution (
  getResolution
  , getResolutionForDisplay
  ) where

import Data.Word
import Foreign.C.Types
import Foreign.Ptr


getResolution :: IO (Int, Int, Int, Int)
getResolution = do
  ddid <- cg_main_display_id
  getResolutionForDisplay (fromIntegral ddid)

-- TODO: currently we just use the "main display" ID.
-- When we return 0 for x and y here, the WebDriver setWindowPos
-- should just position the window on the main display.
-- This probably won't work at all for xvfb displays, but we have more work to do there...
getResolutionForDisplay :: Int -> IO (Int, Int, Int, Int)
getResolutionForDisplay ddid = do
  mode <- cg_display_copy_display_mode (fromIntegral ddid)

  -- This version gets the dimension in "points" (scaled pixels)
  -- width <- cg_display_pixels_wide (fromIntegral ddid)
  -- height <- cg_display_pixels_high (fromIntegral ddid)

  -- This version should always get the dimensions in actual non-scaled pixels
  width <- cg_display_mode_get_pixel_width mode
  height <- cg_display_mode_get_pixel_height mode

  -- We could actually use a combination of cg_display_pixels_wide and cg_display_mode_get_pixel_width to
  -- get the scaling

  return (0, 0, fromIntegral width, fromIntegral height)

type CGDirectDisplayID = Word32

data CGDisplayMode
type CGDisplayModeRef = Ptr CGDisplayMode

foreign import ccall unsafe "<CoreGraphics/CGDisplayConfiguration.h> CGMainDisplayID"
  cg_main_display_id :: IO CGDirectDisplayID

-- foreign import ccall unsafe "<CoreGraphics/CGDisplayConfiguration.h> CGDisplayPixelsWide"
--   cg_display_pixels_wide :: CGDirectDisplayID -> IO CGDirectDisplayID

-- foreign import ccall unsafe "<CoreGraphics/CGDisplayConfiguration.h> CGDisplayPixelsHigh"
--   cg_display_pixels_high :: CGDirectDisplayID -> IO CGDirectDisplayID

foreign import ccall unsafe "CGDisplayModeGetPixelWidth"
  cg_display_mode_get_pixel_width :: CGDisplayModeRef -> IO CSize

foreign import ccall unsafe "CGDisplayModeGetPixelHeight"
  cg_display_mode_get_pixel_height :: CGDisplayModeRef -> IO CSize

foreign import ccall unsafe "CGDisplayCopyDisplayMode"
  cg_display_copy_display_mode :: CGDirectDisplayID -> IO CGDisplayModeRef
