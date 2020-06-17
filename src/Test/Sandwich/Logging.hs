-- |

module Test.Sandwich.Logging where

import Control.Monad.Logger
import Data.Text
import GHC.Stack
import Test.Sandwich.Types.Spec

debug :: (HasCallStack) => Text -> ExampleM context ()
debug = logDebugCS callStack

info :: (HasCallStack) => Text -> ExampleM context ()
info = logInfoCS callStack

warn :: (HasCallStack) => Text -> ExampleM context ()
warn = logWarnCS callStack

logError :: (HasCallStack) => Text -> ExampleM context ()
logError = logErrorCS callStack

logOther :: (HasCallStack) => LogLevel -> Text -> ExampleM context ()
logOther = logOtherCS callStack
