{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Sandwich.WebDriver.Internal.StartWebDriver (
  stopWebDriver
  ) where

import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import GHC.Stack
import Test.Sandwich.WebDriver.Internal.Types
import qualified Test.WebDriver as W

#ifndef mingw32_HOST_OS
import Test.Sandwich.WebDriver.Internal.StartWebDriver.Xvfb
#endif


type Constraints m = (
  HasCallStack, MonadLoggerIO m, MonadUnliftIO m, MonadMask m, MonadFail m
  )

stopWebDriver :: (Constraints m, W.WebDriverBase m) => TestWebDriverContext -> m ()
stopWebDriver (TestWebDriverContext {wdContext}) = do
  W.teardownWebDriverContext wdContext

  -- whenJust maybeXvfbSession $ \(XvfbSession {..}) -> do
  --   whenJust xvfbFluxboxProcess $ \p' -> do
  --     gracefullyStopProcess p' gracePeriod

  --   gracefullyStopProcess xvfbProcess gracePeriod
