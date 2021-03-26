{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Test.Sandwich.Types.ArgParsing where

import Control.Monad.Logger
import GHC.Int


-- * FormatterType

data FormatterType = Print | TUI | Auto | Silent

instance Show FormatterType where
  show Print = "print"
  show TUI = "tui"
  show Auto = "auto"
  show Silent = "silent"

instance Read FormatterType where
  readsPrec _ "print" = [(Print, "")]
  readsPrec _ "tui" = [(TUI, "")]
  readsPrec _ "auto" = [(Auto, "")]
  readsPrec _ "silent" = [(Silent, "")]
  readsPrec _ _ = []

-- * DisplayType

data DisplayType = Current | Headless | Xvfb

instance Show DisplayType where
  show Current = "current"
  show Headless = "headless"
  show Xvfb = "xvfb"

instance Read DisplayType where
  readsPrec _ "current" = [(Current, "")]
  readsPrec _ "headless" = [(Headless, "")]
  readsPrec _ "xvfb" = [(Xvfb, "")]
  readsPrec _ _ = []

-- * CommandLineOptions

data CommandLineOptions a = CommandLineOptions {
  -- sandwich
  optFormatter :: FormatterType
  , optLogLevel :: Maybe LogLevel
  , optTreeFilter :: Maybe String
  , optRepeatCount :: Int
  , optFixedRoot :: Maybe String

  , optListAvailableTests :: Maybe Bool
  , optPrintSlackFlags :: Maybe Bool
  , optPrintWebDriverFlags :: Maybe Bool

  , optIndividualTestModule :: Maybe IndividualTestModule

  , optWebdriverOptions :: CommandLineWebdriverOptions
  , optSlackOptions :: CommandLineSlackOptions

  , optUserOptions :: a
  } deriving Show

data IndividualTestModule = IndividualTestModuleName String
                          | IndividualTestMainFn (IO ())

instance Show IndividualTestModule where
  show (IndividualTestModuleName moduleName) = moduleName
  show (IndividualTestMainFn _) = "<main function>"

-- * sandwich-slack options

data CommandLineSlackOptions = CommandLineSlackOptions {
  optSlackToken :: Maybe String
  , optSlackChannel :: Maybe String

  , optSlackTopMessage :: Maybe String

  , optSlackMaxFailures :: Maybe Int
  , optSlackMaxFailureReasonLines :: Maybe Int
  , optSlackMaxCallStackLines :: Maybe Int

  , optSlackVisibilityThreshold :: Maybe Int

  , optSlackMaxMessageSize :: Maybe Int64
  } deriving Show

-- * sandwich-webdriver options

data BrowserToUse = UseChrome | UseFirefox
  deriving Show

data CommandLineWebdriverOptions = CommandLineWebdriverOptions {
  optFirefox :: BrowserToUse
  , optPoolSize :: Int
  , optDisplay :: DisplayType
  , optFluxbox :: Bool
  , optIndividualVideos :: Bool
  , optErrorVideos :: Bool
  } deriving Show
