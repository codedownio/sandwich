{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Sandwich.WebDriver.Internal.StartWebDriver.Xvfb (
  makeXvfbSession
  ) where

import Control.Exception
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Retry
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate
import GHC.Stack
import Safe
import System.Directory
import System.Environment
import System.IO.Temp
import System.Process
import Test.Sandwich
import Test.Sandwich.WebDriver.Internal.Binaries.Xvfb
import Test.Sandwich.WebDriver.Internal.OnDemand
import Test.Sandwich.WebDriver.Internal.Types
import UnliftIO.MVar


#ifdef linux_HOST_OS
import System.Posix.IO as Posix
import System.Posix.Types
#endif

#ifdef darwin_HOST_OS
import GHC.IO.FD
import GHC.IO.Handle (Handle)
import qualified GHC.IO.Handle.FD as HFD
newtype Fd = Fd FD
handleToFd :: Handle -> IO Fd
handleToFd h = Fd <$> HFD.handleToFd h
#endif

type Constraints m context = (
  HasCallStack, MonadLoggerIO m, MonadUnliftIO m, MonadMask m, MonadFail m
  , MonadReader context m, HasBaseContext context
  )


makeXvfbSession :: (
  Constraints m context
  ) => Maybe (Int, Int) -> Bool -> FilePath -> XvfbToUse -> MVar (OnDemand FilePath) -> m (XvfbSession, [(String, String)])
makeXvfbSession xvfbResolution xvfbStartFluxbox webdriverRoot xvfbOnDemand xvfbToUse = do
  let (w, h) = fromMaybe (1920, 1080) xvfbResolution
  liftIO $ createDirectoryIfMissing True webdriverRoot

  let policy = constantDelay 10000 <> limitRetries 1000
  (serverNum :: Int, p, authFile, displayNum) <- recoverAll policy $ \_ -> do
    withTempFile webdriverRoot "x11_server_num" $ \path tmpHandle -> do
      fd <- liftIO $ handleToFd tmpHandle
      (serverNum, p, authFile) <- createXvfbSession webdriverRoot w h fd xvfbOnDemand xvfbToUse

      debug [i|Trying to determine display number for auth file '#{authFile}', using '#{path}'|]

      displayNum <-
        recoverAll policy $ \_ ->
          (liftIO $ readFile path) >>= \contents -> case readMay contents of -- hGetContents readHandle
            Nothing -> liftIO $ throwIO $ userError [i|Couldn't determine X11 screen to use. Got data: '#{contents}'. Path was '#{path}'|]
            Just (x :: Int) -> return x

      return (serverNum, p, authFile, displayNum)

  fluxboxProcess <- if xvfbStartFluxbox then Just <$> (startFluxBoxOnDisplay webdriverRoot displayNum) else return Nothing

  let xvfbSession = XvfbSession {
        xvfbDisplayNum = displayNum
        , xvfbXauthority = authFile
        , xvfbDimensions = (w, h)
        , xvfbProcess = p
        , xvfbFluxboxProcess = fluxboxProcess
        }

  -- TODO: allow verbose logging to be controlled with an option:
  env' <- liftIO getEnvironment
  let env = L.nubBy (\x y -> fst x == fst y) $ [("DISPLAY", ":" <> show serverNum)
                                               , ("XAUTHORITY", xvfbXauthority xvfbSession)] <> env'
  return (xvfbSession, env)


createXvfbSession :: (
  Constraints m context
  ) => FilePath -> Int -> Int -> Fd -> XvfbToUse -> MVar (OnDemand FilePath) -> m (Int, ProcessHandle, FilePath)
createXvfbSession webdriverRoot w h (Fd fd) xvfbToUse xvfbOnDemand = do
  xvfb <- getOnDemand xvfbOnDemand (obtainXvfb xvfbToUse)

  serverNum <- liftIO findFreeServerNum

  -- Start the Xvfb session
  authFile <- liftIO $ writeTempFile webdriverRoot ".Xauthority" ""
  p <- createProcessWithLogging $ (
    proc xvfb [":" <> show serverNum
              , "-screen", "0", [i|#{w}x#{h}x24|]
              , "-displayfd", [i|#{fd}|]
              , "-auth", authFile
              ]) {
      cwd = Just webdriverRoot
      , create_group = True
      }

  return (serverNum, p, authFile)


findFreeServerNum :: IO Int
findFreeServerNum = findFreeServerNum' 99
  where
    findFreeServerNum' :: Int -> IO Int
    findFreeServerNum' candidate = do
      doesPathExist [i|/tmp/.X11-unix/X#{candidate}|] >>= \case
        True -> findFreeServerNum' (candidate + 1)
        False -> return candidate


startFluxBoxOnDisplay :: Constraints m context => FilePath -> Int -> m ProcessHandle
startFluxBoxOnDisplay webdriverRoot x = do
  logPath <- liftIO $ writeTempFile webdriverRoot "fluxbox.log" ""

  debug [i|Starting fluxbox on logPath '#{logPath}'|]

  let args = ["-display", ":" <> show x
             , "-log", logPath]

  (_, _, _, p) <- liftIO $ createProcess $ (proc "fluxbox" args) {
    cwd = Just webdriverRoot
    , create_group = True
    , std_out = CreatePipe
    , std_err = CreatePipe
    }

  -- TODO: confirm fluxbox started successfully

  return p
