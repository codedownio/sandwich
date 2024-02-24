{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Common
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Logger
import Data.String.Interpolate
import System.Exit
import System.Process
import Test.Sandwich


parallelNDemo :: TopSpec
parallelNDemo = describe "Creating processes with logging" $ do
  it "createProcessWithLogging" $ do
    p <- createProcessWithLogging (shell "echo hiiiiii")
    liftIO (waitForProcess p) >>= (`shouldBe` ExitSuccess)

  it "createProcessWithLogging'" $ do
    p <- createProcessWithLogging' LevelDebug (shell "echo hiiiiii")
    liftIO (waitForProcess p) >>= (`shouldBe` ExitSuccess)

  it "createProcessWithLoggingAndStdin" $ do
    p <- createProcessWithLoggingAndStdin (shell "echo hiiiiii") ""
    liftIO (waitForProcess p) >>= (`shouldBe` ExitSuccess)

  it "createProcessWithLoggingAndStdin'" $ do
    p <- createProcessWithLoggingAndStdin' LevelDebug (shell "echo hiiiiii") ""
    liftIO (waitForProcess p) >>= (`shouldBe` ExitSuccess)

  it "readCreateProcessWithLogging" $ do
    stdout <- readCreateProcessWithLogging (shell ">&2 echo hiiiiii") ""
    info [i|Got stdout: #{stdout}|]

  it "readCreateProcessWithLogging'" $ do
    stdout <- readCreateProcessWithLogging' LevelDebug (shell ">&2 echo hiiiiii") ""
    info [i|Got stdout: #{stdout}|]

  it "callCommandWithLogging" $ do
    callCommandWithLogging ">&2 echo hiiiiii"

  it "callCommandWithLogging'" $ do
    callCommandWithLogging' LevelDebug ">&2 echo hiiiiii"

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
  }

main :: IO ()
main = runSandwichWithCommandLineArgs testOptions parallelNDemo
