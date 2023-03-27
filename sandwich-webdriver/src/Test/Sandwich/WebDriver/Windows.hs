-- | Functions for manipulating browser windows.


module Test.Sandwich.WebDriver.Windows (
  -- * Window positioning
  setWindowLeftSide
  , setWindowRightSide
  , setWindowFullScreen

  -- * Querying screen info
  , getScreenResolution
  ) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Logger (MonadLogger)
import Control.Monad.Reader
import Data.Bits as B
import Data.Maybe
import GHC.Stack
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Resolution
import Test.WebDriver
import qualified Test.WebDriver.Class as W


-- | Position the window on the left 50% of the screen.
setWindowLeftSide :: (HasCallStack, MonadIO wd, WebDriverContext context wd, MonadReader context wd, W.WebDriver wd, MonadLogger wd, MonadMask wd) => wd ()
setWindowLeftSide = do
  sess <- getContext webdriver
  (x, y, width, height) <- case runMode $ wdOptions sess of
    RunHeadless (HeadlessConfig {..}) -> return (0, 0, w, h)
      where (w, h) = fromMaybe (1920, 1080) headlessResolution
    _ -> getScreenResolution sess
  setWindowPos (x, y)
  setWindowSize (fromIntegral $ B.shift width (-1), fromIntegral height)

-- | Position the window on the right 50% of the screen.
setWindowRightSide :: (HasCallStack, MonadIO wd, WebDriverContext context wd, MonadReader context wd, W.WebDriver wd, MonadLogger wd, MonadMask wd) => wd ()
setWindowRightSide = do
  sess <- getContext webdriver
  (x, y, width, height) <- case runMode $ wdOptions sess of
    RunHeadless (HeadlessConfig {..}) -> return (0, 0, w, h)
      where (w, h) = fromMaybe (1920, 1080) headlessResolution
    _ -> getScreenResolution sess
  let pos = (x + (fromIntegral $ B.shift width (-1)), y + 0)
  setWindowPos pos
  setWindowSize (fromIntegral $ B.shift width (-1), fromIntegral height)

-- | Fullscreen the browser window.
setWindowFullScreen :: (HasCallStack, MonadIO wd, WebDriverContext context wd, MonadReader context wd, W.WebDriver wd, MonadLogger wd, MonadMask wd) => wd ()
setWindowFullScreen = do
  sess <- getContext webdriver
  (x, y, width, height) <- case runMode $ wdOptions sess of
    RunHeadless (HeadlessConfig {..}) -> return (0, 0, w, h)
      where (w, h) = fromMaybe (1920, 1080) headlessResolution
    _ -> getScreenResolution sess
  setWindowPos (x + 0, y + 0)
  setWindowSize (fromIntegral width, fromIntegral height)

-- | Get the screen resolution as (x, y, width, height). (The x and y coordinates may be nonzero in multi-monitor setups.)
getScreenResolution :: (HasCallStack, MonadIO m, MonadMask m, MonadLogger m) => WebDriver -> m (Int, Int, Int, Int)
getScreenResolution (WebDriver {wdWebDriver=(_, _, _, _, _, maybeXvfbSession)}) = case maybeXvfbSession of
  Nothing -> liftIO getResolution
  Just (XvfbSession {..}) -> liftIO $ getResolutionForDisplay xvfbDisplayNum
