-- |

module Test.Sandwich.Logging where

import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.Text
import GHC.Stack
import Test.Sandwich.Types.Spec

debug :: (HasCallStack, MonadIO m) => Text -> ExampleT context m ()
debug = logDebugCS callStack

info :: (HasCallStack, MonadIO m) => Text -> ExampleT context m ()
info = logInfoCS callStack

warn :: (HasCallStack, MonadIO m) => Text -> ExampleT context m ()
warn = logWarnCS callStack

logError :: (HasCallStack, MonadIO m) => Text -> ExampleT context m ()
logError = logErrorCS callStack

logOther :: (HasCallStack, MonadIO m) => LogLevel -> Text -> ExampleT context m ()
logOther = logOtherCS callStack
