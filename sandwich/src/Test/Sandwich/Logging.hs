{-# LANGUAGE CPP #-}

-- | Logging functions.

module Test.Sandwich.Logging (
  debug
  , info
  , warn
  , Test.Sandwich.Logging.logError
  , Test.Sandwich.Logging.logOther

  -- * Process functions with direct logging
  , createProcessWithLogging
  , readCreateProcessWithLogging
  , createProcessWithLoggingAndStdin
  , callCommandWithLogging

  , createProcessWithLogging'
  , readCreateProcessWithLogging'
  , createProcessWithLoggingAndStdin'
  , callCommandWithLogging'

  -- * Process functions with file logging
  , createProcessWithFileLogging
  , readCreateProcessWithFileLogging
  , createProcessWithFileLoggingAndStdin
  , callCommandWithFileLogging

  , createProcessWithFileLogging'
  , readCreateProcessWithFileLogging'
  , createProcessWithFileLoggingAndStdin'
  , callCommandWithFileLogging'
  ) where

import Control.Monad.Logger hiding (logOther)
import Data.Text (Text)
import GHC.Stack
import Test.Sandwich.Logging.Process
import Test.Sandwich.Logging.ProcessFileLogging


-- * Basic logging functions


-- | Log a message at level 'LevelDebug'.
debug :: (HasCallStack, MonadLogger m) => Text -> m ()
debug = logDebugCS callStack

-- | Log a message at level 'LevelInfo'.
info :: (HasCallStack, MonadLogger m) => Text -> m ()
info = logInfoCS callStack

-- | Log a message at level 'LevelWarn'.
warn :: (HasCallStack, MonadLogger m) => Text -> m ()
warn = logWarnCS callStack

-- | Log a message at level 'LevelError'.
logError :: (HasCallStack, MonadLogger m) => Text -> m ()
logError = logErrorCS callStack

-- | Log with a custom 'LogLevel'.
logOther :: (HasCallStack, MonadLogger m) => LogLevel -> Text -> m ()
logOther = logOtherCS callStack
