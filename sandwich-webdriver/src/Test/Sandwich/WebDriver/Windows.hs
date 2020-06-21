{-# LANGUAGE QuasiQuotes #-}
-- | Functions for manipulating browser windows

module Test.Sandwich.WebDriver.Windows (
  setWindowLeftSide
  , setWindowRightSide
  , setWindowFullScreen
  ) where

import Control.Monad.IO.Class
import Data.Bits as B
import qualified Data.List as L
import Data.String
import Data.String.Interpolate.IsString
import qualified Data.Text as T
import GHC.Stack
import System.Environment
import System.Process
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Types
import Test.WebDriver
import Test.WebDriver.Class


setWindowLeftSide :: (HasCallStack, MonadIO wd, HasWebDriver context wd) => ExampleT context wd ()
setWindowLeftSide = do
  sess <- getContext webdriver
  (width, height) <- liftIO $ getScreenResolution sess
  setWindowPos (0, 0)
  setWindowSize (fromIntegral $ B.shift width (-1), fromIntegral height)

setWindowRightSide :: (HasCallStack, MonadIO wd, HasWebDriver context wd) => ExampleT context wd ()
setWindowRightSide = do
  sess <- getContext webdriver
  (width, height) <- liftIO $ getScreenResolution sess
  let pos = (fromIntegral $ B.shift width (-1), 0)
  setWindowPos pos
  setWindowSize (fromIntegral $ B.shift width (-1), fromIntegral height)

setWindowFullScreen :: (HasCallStack, MonadIO wd, HasWebDriver context wd) => ExampleT context wd ()
setWindowFullScreen = do
  sess <- getContext webdriver
  (width, height) <- liftIO $ getScreenResolution sess
  setWindowPos (0, 0)
  setWindowSize (fromIntegral width, fromIntegral height)

-- * Internal

getScreenResolution :: (HasCallStack) => WdSession -> IO (Int, Int)
getScreenResolution (WdSession {wdWebDriver=(_, _, _, _, _, maybeXvfbSession)}) = do
  envArg <- case maybeXvfbSession of
    Nothing -> return Nothing
    Just (XvfbSession {..}) -> do
      -- Use same environment as shell, but replace DISPLAY arg
      env' <- liftIO getEnvironment
      return $ Just $ L.nubBy (\x y -> fst x == fst y) $ [("DISPLAY", ":" <> show xvfbDisplayNum)
                                                         , ("XAUTHORITY", xvfbXauthority)] <> env'

  result <- readCreateProcess ((shell "xdpyinfo | grep dimensions") { env=envArg }) ""
  let resolutionPart = (T.words $ fromString result) !! 1
  let (widthText:heightText:_) = T.splitOn "x" resolutionPart
  return (read $ T.unpack widthText, read $ T.unpack heightText)
