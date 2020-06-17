-- |

module Test.Sandwich.Logging where

import Control.Monad.Logger.CallStack
import Data.Text
import GHC.Stack
import Test.Sandwich.Types.Spec

debug :: (HasCallStack) => Text -> ExampleM context ()
debug = logDebugN

info :: (HasCallStack) => Text -> ExampleM context ()
info = logInfoN

warn :: (HasCallStack) => Text -> ExampleM context ()
warn = logWarnN

logError :: (HasCallStack) => Text -> ExampleM context ()
logError = logErrorN

logOther :: (HasCallStack) => LogLevel -> Text -> ExampleM context ()
logOther = logOtherN
