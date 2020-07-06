{-# LANGUAGE QuasiQuotes #-}
-- | Functions for manipulating browser windows

module Test.Sandwich.WebDriver.Windows (
  setWindowLeftSide
  , setWindowRightSide
  , setWindowFullScreen
  ) where

import Control.Exception.Safe
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Bits as B
import qualified Data.List as L
import Data.String
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import GHC.Stack
import qualified Graphics.X11.Xinerama as X
import qualified Graphics.X11.Xlib.Display as X
import Safe
import System.Environment
import System.Process
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Types
import Test.WebDriver
import Test.WebDriver.Class


setWindowLeftSide :: (HasCallStack, MonadIO wd, HasWebDriver context wd, MonadReader context wd, WebDriver wd) => wd ()
setWindowLeftSide = do
  sess <- getContext webdriver
  (x, y, width, height) <- liftIO $ getScreenResolutionX11 sess
  setWindowPos (x + 0, y + 0)
  setWindowSize (fromIntegral $ B.shift width (-1), fromIntegral height)

setWindowRightSide :: (HasCallStack, MonadIO wd, HasWebDriver context wd, MonadReader context wd, WebDriver wd) => wd ()
setWindowRightSide = do
  sess <- getContext webdriver
  (x, y, width, height) <- liftIO $ getScreenResolutionX11 sess
  let pos = (x + (fromIntegral $ B.shift width (-1)), y + 0)
  setWindowPos pos
  setWindowSize (fromIntegral $ B.shift width (-1), fromIntegral height)

setWindowFullScreen :: (HasCallStack, MonadIO wd, HasWebDriver context wd, MonadReader context wd, WebDriver wd) => wd ()
setWindowFullScreen = do
  sess <- getContext webdriver
  (x, y, width, height) <- liftIO $ getScreenResolutionX11 sess
  setWindowPos (x + 0, y + 0)
  setWindowSize (fromIntegral width, fromIntegral height)

-- * Internal

-- * Getting screen resolution by shelling out to xdpyinfo

getScreenResolutionXdpyinfo :: (HasCallStack) => WdSession -> IO (Int, Int)
getScreenResolutionXdpyinfo (WdSession {wdWebDriver=(_, _, _, _, _, maybeXvfbSession)}) = do
  getScreenResolutionXdpyinfoWithEnv =<< case maybeXvfbSession of
    Nothing -> return Nothing
    Just (XvfbSession {..}) -> do
      -- Use same environment as shell, but replace DISPLAY arg
      env' <- liftIO getEnvironment
      return $ Just $ L.nubBy (\x y -> fst x == fst y) $ [("DISPLAY", ":" <> show xvfbDisplayNum)
                                                         , ("XAUTHORITY", xvfbXauthority)] <> env'

getScreenResolutionXdpyinfoWithEnv :: (HasCallStack) => Maybe [(String, String)] -> IO (Int, Int)
getScreenResolutionXdpyinfoWithEnv envArg = do
  result <- readCreateProcess ((shell "xdpyinfo | grep dimensions") { env=envArg }) ""
  let resolutionPart = (T.words $ fromString result) !! 1
  let (widthText:heightText:_) = T.splitOn "x" resolutionPart
  return (read $ T.unpack widthText, read $ T.unpack heightText)

-- * Getting screen resolution with X11 library

getScreenResolutionX11 :: (HasCallStack) => WdSession -> IO (Int, Int, Int, Int)
getScreenResolutionX11 (WdSession {wdWebDriver=(_, _, _, _, _, maybeXvfbSession)}) = case maybeXvfbSession of
    Nothing -> getScreenResolutionX11' ":0" 0
    Just (XvfbSession {..}) -> getScreenResolutionX11' (":" <> show xvfbDisplayNum) 0

getScreenResolutionX11' :: (HasCallStack) => String -> Int -> IO (Int, Int, Int, Int)
getScreenResolutionX11' displayString screenNumber = do
  bracket (X.openDisplay displayString) X.closeDisplay $ \display -> do
    X.xineramaQueryScreens display >>= \case
      Nothing -> throwIO $ userError [i|Couldn't query X11 for screens for display "#{display}"|]
      Just infos -> do
        case headMay [(xsi_x_org, xsi_y_org, xsi_width, xsi_height) | X.XineramaScreenInfo {..} <- infos
                                                                    , xsi_screen_number == fromIntegral screenNumber] of
          Nothing -> throwIO $ userError [i|Failed to get screen resolution (couldn't find screen number #{screenNumber})|]
          Just (x, y, w, h) -> return (fromIntegral x, fromIntegral y, fromIntegral w, fromIntegral h)
