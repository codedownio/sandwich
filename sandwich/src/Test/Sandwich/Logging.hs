-- |

module Test.Sandwich.Logging where

import Control.Monad.Logger
import Data.Text
import GHC.Stack

debug :: (HasCallStack, MonadLogger m) => Text -> m ()
debug = logDebugCS callStack

info :: (HasCallStack, MonadLogger m) => Text -> m ()
info = logInfoCS callStack

warn :: (HasCallStack, MonadLogger m) => Text -> m ()
warn = logWarnCS callStack

logError :: (HasCallStack, MonadLogger m) => Text -> m ()
logError = logErrorCS callStack

logOther :: (HasCallStack, MonadLogger m) => LogLevel -> Text -> m ()
logOther = logOtherCS callStack
