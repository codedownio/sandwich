{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
module Main where

import Brick
import Data.Text as T
import Data.Time.Clock
import GHC.Stack
import Graphics.Vty.Attributes
import Test.Sandwich
import Test.Sandwich.Formatters.TerminalUI


data MyException = MyException Text CallStack
  deriving Show
instance Exception MyException

data MyColoredException = MyColoredException Text
  deriving Show
instance Exception MyColoredException

customExceptionsDemo :: TopSpec
customExceptionsDemo = describe "Custom exceptions" $ do
  it "formats a custom exception with message and callstack" $ do
    throwIO $ MyException "My message" callStack

  it "formats a custom exception with its own widget rendering function" $ do
    throwIO $ MyColoredException "My widget message"

formatMyException :: SomeException -> Maybe CustomTUIException
formatMyException e = case fromException e of
  Just (MyException msg cs) -> Just $ CustomTUIExceptionMessageAndCallStack msg (Just cs)
  Nothing -> Nothing

formatMyColoredException :: SomeException -> Maybe CustomTUIException
formatMyColoredException e = case fromException e of
  Just (MyColoredException msg) -> Just $ CustomTUIExceptionBrick (modifyDefAttr (\x -> x { attrForeColor = SetTo blue }) (str $ T.unpack msg))
  Nothing -> Nothing

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
  , optionsFormatters = [SomeFormatter $ defaultTerminalUIFormatter {
      terminalUICustomExceptionFormatters = [formatMyException, formatMyColoredException]
      }]
  }

main :: IO ()
main = runSandwich testOptions customExceptionsDemo
