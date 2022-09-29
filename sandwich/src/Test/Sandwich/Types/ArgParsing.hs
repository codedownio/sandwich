{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

module Test.Sandwich.Types.ArgParsing where

import Control.Monad.Logger
import GHC.Int


-- * FormatterType

data FormatterType =
  Print
  | PrintFailures
  | Auto
  | Silent
#ifndef mingw32_HOST_OS
  | TUI
#endif

instance Show FormatterType where
  show Print = "print"
  show PrintFailures = "print-failures"
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
  optFormatter :: FormatterType
  , optLogLevel :: Maybe LogLevel
  , optVisibilityThreshold :: Maybe Int
  , optTreeFilter :: [String]
  , optRepeatCount :: Int
  , optFixedRoot :: Maybe String
  , optDryRun :: Maybe Bool
  , optMarkdownSummaryPath :: Maybe FilePath

  , optListAvailableTests :: Maybe Bool
  , optPrintGoldenFlags :: Maybe Bool
  , optPrintQuickCheckFlags :: Maybe Bool
  , optPrintHedgehogFlags :: Maybe Bool
  , optPrintSlackFlags :: Maybe Bool
  , optPrintWebDriverFlags :: Maybe Bool

  , optIndividualTestModule :: Maybe IndividualTestModule

  , optGoldenOptions :: CommandLineGoldenOptions
  , optQuickCheckOptions :: CommandLineQuickCheckOptions
  , optHedgehogOptions :: CommandLineHedgehogOptions
  , optSlackOptions :: CommandLineSlackOptions
  , optWebdriverOptions :: CommandLineWebdriverOptions

  , optUserOptions :: a
  } deriving Show

data IndividualTestModule = IndividualTestModuleName String
                          | IndividualTestMainFn (IO ())

instance Show IndividualTestModule where
  show (IndividualTestModuleName moduleName) = moduleName
  show (IndividualTestMainFn _) = "<main function>"

-- * golden options

data CommandLineGoldenOptions = CommandLineGoldenOptions {
  optUpdateGolden :: Maybe Bool
  , optGoldenDir :: Maybe FilePath
  } deriving Show

-- * sandwich-quickcheck options

data CommandLineQuickCheckOptions = CommandLineQuickCheckOptions {
  optQuickCheckSeed :: Maybe Integer
  , optQuickCheckMaxDiscardRatio :: Maybe Int
  , optQuickCheckMaxSize :: Maybe Int
  , optQuickCheckMaxSuccess :: Maybe Int
  , optQuickCheckMaxShrinks :: Maybe Int
  } deriving Show

-- * sandwich-hedgehog options

data CommandLineHedgehogOptions = CommandLineHedgehogOptions {
  optHedgehogSeed :: Maybe String
  , optHedgehogSize :: Maybe Int
  , optHedgehogDiscardLimit :: Maybe Integer
  , optHedgehogShrinkLimit :: Maybe Integer
  , optHedgehogShrinkRetries :: Maybe Integer
  } deriving Show

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
  optFirefox :: Maybe BrowserToUse
  , optDisplay :: Maybe DisplayType
  , optFluxbox :: Bool
  , optIndividualVideos :: Bool
  , optErrorVideos :: Bool
  } deriving Show
