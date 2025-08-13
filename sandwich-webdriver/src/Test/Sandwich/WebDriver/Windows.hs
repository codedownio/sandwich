-- | Functions for manipulating browser windows.

module Test.Sandwich.WebDriver.Windows (
  -- * Window positioning
  setWindowLeftSide
  , setWindowRightSide
  , setWindowFullScreen

  -- * Screen resolution
  , getScreenResolution

  -- * Lower-level
  , getResolution
  , getResolutionForDisplay
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Maybe
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Resolution
import Test.Sandwich.WebDriver.Types
import Test.WebDriver


-- | Position the window on the left 50% of the screen.
setWindowLeftSide :: (WebDriverMonad m context, MonadReader context m, WebDriver m) => m ()
setWindowLeftSide = do
  sess <- getContext webdriver
  (x, y, width, height) <- case runMode $ wdOptions sess of
    RunHeadless (HeadlessConfig {..}) -> return (0, 0, w, h)
      where (w, h) = fromMaybe (1920, 1080) headlessResolution
    _ -> getScreenResolution sess

  (screenWidth, screenHeight) <- getScreenPixelDimensions width height

  setWindowRect $ Rect (fromIntegral x) (fromIntegral y) (realToFrac (screenWidth / 2.0)) (realToFrac screenHeight)

-- | Position the window on the right 50% of the screen.
setWindowRightSide :: (WebDriverMonad m context, MonadReader context m, WebDriver m) => m ()
setWindowRightSide = do
  sess <- getContext webdriver
  (x, y, width, height) <- case runMode $ wdOptions sess of
    RunHeadless (HeadlessConfig {..}) -> return (0, 0, w, h)
      where (w, h) = fromMaybe (1920, 1080) headlessResolution
    _ -> getScreenResolution sess

  (screenWidth, screenHeight) <- getScreenPixelDimensions width height

  setWindowRect $ Rect (fromIntegral (x + round (screenWidth / 2.0))) (fromIntegral (y + 0)) (realToFrac (screenWidth / 2.0)) (realToFrac screenHeight)

-- | Fullscreen the browser window.
setWindowFullScreen :: (WebDriverMonad m context, MonadReader context m, WebDriver m) => m ()
setWindowFullScreen = do
  sess <- getContext webdriver
  (x, y, width, height) <- case runMode $ wdOptions sess of
    RunHeadless (HeadlessConfig {..}) -> return (0, 0, w, h)
      where (w, h) = fromMaybe (1920, 1080) headlessResolution
    _ -> getScreenResolution sess

  (screenWidth, screenHeight) <- getScreenPixelDimensions width height

  setWindowRect $ Rect (fromIntegral x) (fromIntegral y) (realToFrac screenWidth) (realToFrac screenHeight)

-- | Get the screen resolution as (x, y, width, height). (The x and y coordinates may be nonzero in multi-monitor setups.)
-- This function works with both normal 'RunMode' and Xvfb mode.
getScreenResolution :: (MonadIO m) => TestWebDriverContext -> m (Int, Int, Int, Int)
-- getScreenResolution (TestWebDriverContext {wdWebDriver=(_, maybeXvfbSession)}) = case maybeXvfbSession of
--   Nothing -> liftIO getResolution
--   Just (XvfbSession {..}) -> liftIO $ getResolutionForDisplay xvfbDisplayNum
getScreenResolution twdc = liftIO getResolution

getScreenPixelDimensions :: (WebDriver m) => Int -> Int -> m (Double, Double)
getScreenPixelDimensions width height = do
  devicePixelRatio <- executeJS [] "return window.devicePixelRatio" >>= \case
    Just (ratio :: Double) -> pure ratio
    Nothing -> pure 1.0

  let screenWidth = fromIntegral width / devicePixelRatio
  let screenHeight = fromIntegral height / devicePixelRatio

  return (screenWidth, screenHeight)
