-- | Functions for manipulating browser windows.

module Test.Sandwich.WebDriver.Windows (
  -- * Window positioning
  setWindowLeftSide
  , setWindowRightSide
  , setWindowFullScreen

  -- * Querying screen info
  , getScreenResolution
  ) where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Maybe
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Resolution
import Test.Sandwich.WebDriver.Types
import Test.WebDriver
import qualified Test.WebDriver.Class as W


-- | Position the window on the left 50% of the screen.
setWindowLeftSide :: (WebDriverMonad m context, MonadReader context m, W.WebDriver m) => m ()
setWindowLeftSide = do
  sess <- getContext webdriver
  (x, y, width, height) <- case runMode $ wdOptions sess of
    RunHeadless (HeadlessConfig {..}) -> return (0, 0, w, h)
      where (w, h) = fromMaybe (1920, 1080) headlessResolution
    _ -> getScreenResolution sess

  (screenWidth, screenHeight) <- getScreenPixelDimensions width height

  setWindowPos (x, y)
  setWindowSize (round (screenWidth / 2.0), round screenHeight)

-- | Position the window on the right 50% of the screen.
setWindowRightSide :: (WebDriverMonad m context, MonadReader context m, W.WebDriver m) => m ()
setWindowRightSide = do
  sess <- getContext webdriver
  (x, y, width, height) <- case runMode $ wdOptions sess of
    RunHeadless (HeadlessConfig {..}) -> return (0, 0, w, h)
      where (w, h) = fromMaybe (1920, 1080) headlessResolution
    _ -> getScreenResolution sess

  (screenWidth, screenHeight) <- getScreenPixelDimensions width height

  setWindowPos (x + round (screenWidth / 2.0), y + 0)
  setWindowSize (round (screenWidth / 2.0), round screenHeight)

-- | Fullscreen the browser window.
setWindowFullScreen :: (WebDriverMonad m context, MonadReader context m, W.WebDriver m) => m ()
setWindowFullScreen = do
  sess <- getContext webdriver
  (x, y, width, height) <- case runMode $ wdOptions sess of
    RunHeadless (HeadlessConfig {..}) -> return (0, 0, w, h)
      where (w, h) = fromMaybe (1920, 1080) headlessResolution
    _ -> getScreenResolution sess

  (screenWidth, screenHeight) <- getScreenPixelDimensions width height

  setWindowPos (x + 0, y + 0)
  setWindowSize (round screenWidth, round screenHeight)

-- | Get the screen resolution as (x, y, width, height). (The x and y coordinates may be nonzero in multi-monitor setups.)
getScreenResolution :: (MonadIO m) => WebDriver -> m (Int, Int, Int, Int)
getScreenResolution (WebDriver {wdWebDriver=(_, maybeXvfbSession)}) = case maybeXvfbSession of
  Nothing -> liftIO getResolution
  Just (XvfbSession {..}) -> liftIO $ getResolutionForDisplay xvfbDisplayNum

getScreenPixelDimensions :: (MonadIO m, W.WebDriver m) => Int -> Int -> m (Double, Double)
getScreenPixelDimensions width height = do
  devicePixelRatio <- executeJS [] "return window.devicePixelRatio" >>= \case
    Just (ratio :: Double) -> pure ratio
    Nothing -> pure 1.0

  let screenWidth = fromIntegral width / devicePixelRatio
  let screenHeight = fromIntegral height / devicePixelRatio

  return (screenWidth, screenHeight)
