{-# LANGUAGE ViewPatterns, LambdaCase, QuasiQuotes, RecordWildCards, NamedFieldPuns, ScopedTypeVariables #-}
-- |

module Test.Sandwich.WebDriver.Internal.Action where

import Control.Concurrent.MVar.Lifted
import qualified Control.Exception.Lifted as EL
import Control.Exception.Safe
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Trans.Control (MonadBaseControl)
import qualified Data.Map as M
import Data.String.Interpolate.IsString
import GHC.Stack
import Test.Sandwich.Logging
import Test.Sandwich.WebDriver.Internal.Types
import Test.Sandwich.WebDriver.Internal.Util
import qualified Test.WebDriver as W
import qualified Test.WebDriver.Config as W
import qualified Test.WebDriver.Session as W

runActionWithBrowser :: (HasCallStack) => Browser -> W.WD a -> WdSession -> IO a
runActionWithBrowser browser action (WdSession {..}) = do
  -- Create new session if necessary (this can throw an exception)
  sess <- modifyMVar wdSessionMap $ \sessionMap -> case M.lookup browser sessionMap of
    Just sess -> return (sessionMap, sess)
    Nothing -> do
      sess'' <- W.mkSession wdConfig
      let sess' = sess'' { W.wdSessHistUpdate = W.unlimitedHistory }
      sess <- W.runWD sess' $ W.createSession $ W.wdCapabilities wdConfig
      return (M.insert browser sess sessionMap, sess)

  -- Run the test example, handling the exception specially
  (liftIO $ tryAny $ W.runWD sess $ do
      -- After the action, grab the updated session and save it before we return
      EL.finally action $ do
        sess' <- W.getSession
        liftIO $ modifyMVar_ wdSessionMap $ return . M.insert browser sess'
    ) >>= \case
    Left e -> liftIO $ do
      -- handleTestException sessionWithLabels e
      throw e -- Rethrow for the test framework to handle
    Right x -> return x

runWithBrowser' :: (HasCallStack, HasWdSession a) => Browser -> W.WD () -> a -> IO ()
runWithBrowser' browser action hasSession = do
  runActionWithBrowser browser action (getWdSession hasSession)

runEveryBrowser' :: (HasCallStack, HasWdSession a) => W.WD () -> a -> IO ()
runEveryBrowser' action (getWdSession -> session@(WdSession {wdSessionMap})) = do
  sessionMap <- readMVar wdSessionMap
  forM_ (M.toList sessionMap) $ \(browser, _) -> do
    runActionWithBrowser browser action session

executeWithBrowser :: (HasCallStack) => Browser -> WdSession -> W.WD a -> W.WD a
executeWithBrowser browser session action = do
  liftIO $ runActionWithBrowser browser action session

closeSession :: (HasCallStack, MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m) => Browser -> WdSession -> m ()
closeSession browser (WdSession {wdSessionMap}) = do
  modifyMVar_ wdSessionMap $ \sessionMap -> do
    whenJust (M.lookup browser sessionMap) $ \sess ->
      liftIO $ W.runWD sess W.closeSession
    return $ M.delete browser sessionMap

closeAllSessionsExcept :: (HasCallStack, MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m) => [Browser] -> WdSession -> m ()
closeAllSessionsExcept toKeep (WdSession {wdSessionMap}) = do
  modifyMVar_ wdSessionMap $ \sessionMap -> do
    forM_ (M.toList sessionMap) $ \(name, sess) -> unless (name `elem` toKeep) $
      catch (liftIO $ W.runWD sess W.closeSession)
            (\(e :: SomeException) -> warn [i|Failed to destroy session '#{name}': '#{e}'|])
    return $ M.fromList [(b, s) | (b, s) <- M.toList sessionMap, b `elem` toKeep]

closeAllSessions :: (HasCallStack, MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadCatch m) => WdSession -> m ()
closeAllSessions = closeAllSessionsExcept []
