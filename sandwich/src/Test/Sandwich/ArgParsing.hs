{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Test.Sandwich.ArgParsing where

import Control.Monad.Logger
import Data.Maybe
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Options.Applicative
import qualified Options.Applicative as OA
import System.IO
import Test.Sandwich.Formatters.LogSaver
import Test.Sandwich.Formatters.Print.Types
import Test.Sandwich.Formatters.TerminalUI
import Test.Sandwich.Formatters.TerminalUI.Types
import Test.Sandwich.Options
import Test.Sandwich.Types.RunTree

#if MIN_VERSION_time(1,9,0)
import Data.Time.Format.ISO8601
formatTime = T.unpack . T.replace ":" "_" . T.pack . iso8601Show
#else
formatTime = show
#endif


commandLineOptionsWithInfo :: ParserInfo CommandLineOptions
commandLineOptionsWithInfo = OA.info (commandLineOptions <**> helper)
  (
    fullDesc
    <> progDesc "Run tests with Sandwich"
    <> header "Sandwich test runner"
  )

-- * FormatterType

data FormatterType = Print | TUI | Auto

instance Show FormatterType where
  show Print = "print"
  show TUI = "tui"
  show Auto = "auto"

instance Read FormatterType where
  readsPrec _ "print" = [(Print, "")]
  readsPrec _ "tui" = [(TUI, "")]
  readsPrec _ "auto" = [(Auto, "")]
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

data CommandLineOptions = CommandLineOptions {
  -- sandwich
  optFormatter :: FormatterType
  , optLogLevel :: Maybe LogLevel
  , optTreeFilter :: Maybe String
  , optRepeatCount :: Int
  , optFixedRoot :: Maybe String

  , optWebdriverOptions :: CommandLineWebdriverOptions
  , optSlackOptions :: CommandLineSlackOptions
  } deriving Show

commandLineOptions :: Parser CommandLineOptions
commandLineOptions = CommandLineOptions
  -- sandwich
  <$> formatter
  <*> logLevel
  <*> optional (strOption (long "filter" <> short 'f' <> help "Filter test tree by string matching text example labels" <> metavar "STRING"))
  <*> option auto (long "repeat" <> short 'r' <> showDefault <> help "Repeat the test N times and report how many failures occur" <> value 1 <> metavar "INT")
  <*> optional (strOption (long "fixed-root" <> help "Store test artifacts at a fixed path" <> metavar "STRING"))

  <*> commandLineWebdriverOptions
  <*> commandLineSlackOptions

formatter :: Parser FormatterType
formatter =
  flag' Print (long "print" <> help "Print to stdout")
  <|> flag' TUI (long "tui" <> help "Open terminal UI app")
  <|> flag Auto Auto (long "auto" <> help "Automatically decide which formatter to use")

logLevel :: Parser (Maybe LogLevel)
logLevel =
  flag' (Just LevelDebug) (long "debug" <> help "Log level DEBUG")
  <|> flag' (Just LevelInfo) (long "info" <> help "Log level INFO")
  <|> flag' (Just LevelWarn) (long "warn" <> help "Log level WARN")
  <|> flag (Just LevelWarn) (Just LevelError) (long "error" <> help "Log level ERROR")

-- * sandwich-webdriver options

data CommandLineWebdriverOptions = CommandLineWebdriverOptions {
  optFirefox :: Bool
  , optPoolSize :: Int
  , optDisplay :: DisplayType
  , optFluxbox :: Bool
  , optIndividualVideos :: Bool
  , optErrorVideos :: Bool
  } deriving Show

commandLineWebdriverOptions :: Parser CommandLineWebdriverOptions
commandLineWebdriverOptions = CommandLineWebdriverOptions
  <$> flag False True (long "firefox" <> help "(sandwich-webdriver) Use Firefox for Selenium tests (instead of the default of Chrome).")
  <*> option auto (long "pool-size" <> short 'p' <> showDefault <> help "(sandwich-webdriver) WebDriver pool size" <> value 4 <> metavar "INT")
  <*> display
  <*> flag False True (long "fluxbox" <> help "(sandwich-webdriver) Launch fluxbox as window manager when using Xvfb")
  <*> flag False True (long "individual-videos" <> help "(sandwich-webdriver) Record individual videos of each test.")
  <*> flag False True (long "error-videos" <> help "(sandwich-webdriver) Record videos of each test but delete them unless there was an exception")

display :: Parser DisplayType
display =
  flag' Current (long "current" <> help "(sandwich-webdriver) Open browser in current display")
  <|> flag' Headless (long "headless" <> help "(sandwich-webdriver) Run browser in headless mode")
  <|> flag Current Xvfb (long "xvfb" <> help "(sandwich-webdriver) Run browser in Xvfb session")

-- * sandwich-slack options

data CommandLineSlackOptions = CommandLineSlackOptions {
  optSlackToken :: Maybe String
  , optSlackChannel :: Maybe String
  , optSlackTopMessage :: Maybe String
  } deriving Show

commandLineSlackOptions :: Parser CommandLineSlackOptions
commandLineSlackOptions = CommandLineSlackOptions
  <$> optional (strOption (long "slack-token" <> help "(sandwich-slack) Slack token to use with the Slack formatter" <> metavar "STRING"))
  <*> optional (strOption (long "slack-channel" <> help "(sandwich-slack) Slack channel to use with the Slack formatter" <> metavar "STRING"))
  <*> optional (strOption (long "slack-top-message" <> help "(sandwich-slack) Top message to display on Slack progress bars" <> metavar "STRING"))

-- * Main parsing function

addOptionsFromArgs :: Options -> IO (Options, Int)
addOptionsFromArgs baseOptions = do
  CommandLineOptions {..} <- OA.execParser commandLineOptionsWithInfo

  let printFormatter = SomeFormatter $ defaultPrintFormatter { printFormatterLogLevel = optLogLevel }
  let tuiFormatter = SomeFormatter $ defaultTerminalUIFormatter { terminalUILogLevel = optLogLevel }

  mainFormatter <- case (optRepeatCount, optFormatter) of
    (x, _) | x /= 1 -> return printFormatter
    (_, Auto) -> hIsTerminalDevice stdout >>= \case
      True -> return printFormatter
      False -> return tuiFormatter
    (_, TUI) -> return tuiFormatter
    (_, Print) -> return printFormatter

  -- let slackFormatter = case (optSlackToken, optSlackChannel) of
  --       (Just token, Just channel) -> Just $ SomeFormatter $ defaultSlackFormatter {
  --         slackFormatterSlackConfig = SlackConfig (convert token)
  --         , slackFormatterTopMessage = optSlackTopMessage
  --         , slackFormatterChannel = channel
  --         , slackFormatterVisibilityThreshold = Just 50
  --         }
  --       _ -> Nothing

  let options = baseOptions {
    optionsTestArtifactsDirectory = case optFixedRoot of
      Nothing -> TestArtifactsGeneratedDirectory "test_runs" (formatTime <$> getCurrentTime)
      Just path -> TestArtifactsFixedDirectory path
    , optionsFilterTree = TreeFilter <$> optTreeFilter
    , optionsFormatters = catMaybes [Just mainFormatter, Just $ SomeFormatter defaultLogSaverFormatter]
    -- , optionsFormatters = catMaybes [Just mainFormatter, Just $ SomeFormatter defaultLogSaverFormatter, slackFormatter]
    }

  -- let runMode = case optDisplay of
  --       Headless -> RunHeadless defaultHeadlessConfig
  --       Xvfb -> RunInXvfb (defaultXvfbConfig { xvfbStartFluxbox = optFluxbox })
  --       Current -> Normal

  -- let toolsRoot = "/tmp/tools"
  -- seleniumPath <- (runStdoutLoggingT $ obtainSelenium toolsRoot DownloadSeleniumDefault) >>= \case
  --   Left err -> error [i|Failed to get selenium JAR: #{err}|]
  --   Right x -> return x
  -- chromeDriverPath <- (runStdoutLoggingT $ obtainChromeDriver toolsRoot DownloadChromeDriverAutodetect) >>= \case
  --   Left err -> error [i|Failed to get chromedriver: #{err}|]
  --   Right x -> return x
  -- geckoDriverPath <- (runStdoutLoggingT $ obtainGeckoDriver toolsRoot DownloadGeckoDriverAutodetect) >>= \case
  --   Left err -> error [i|Failed to get geckodriver: #{err}|]
  --   Right x -> return x

  -- httpManager <- newManager (defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro 60000000 })

  -- let wdOptions = (defaultWdOptions "/tmp/tools") {
  --   capabilities = if optFirefox then firefoxCapabilities else chromeCapabilities
  --   , saveSeleniumMessageHistory = Always
  --   , seleniumToUse = UseSeleniumAt seleniumPath
  --   , chromeDriverToUse = UseChromeDriverAt chromeDriverPath
  --   , geckoDriverToUse = UseGeckoDriverAt geckoDriverPath
  --   , runMode = runMode
  --   , httpManager = Just httpManager
  --   , httpRetryCount = 3
  --   }

  return (options, optRepeatCount)
