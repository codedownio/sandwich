{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}

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
import Test.Sandwich.Options

#if MIN_VERSION_time(1,9,0)
import Data.Time.Format.ISO8601
formatTime = T.unpack . T.replace ":" "_" . T.pack . iso8601Show
#else
formatTime = show
#endif


commandLineOptionsWithInfo :: Parser a -> ParserInfo (CommandLineOptions a)
commandLineOptionsWithInfo userOptionsParser = OA.info (mainCommandLineOptions userOptionsParser <**> helper)
  (
    fullDesc
    <> progDesc "Run tests with Sandwich"
    <> header "Sandwich test runner"
  )

slackOptionsWithInfo :: ParserInfo CommandLineSlackOptions
slackOptionsWithInfo = OA.info (commandLineSlackOptions mempty <**> helper)
  (
    briefDesc
    <> header "Special options passed to the Slack formatter.\n\nIf a flag is passed, it will override the value in the SlackFormatter configured in the code."
  )

webDriverOptionsWithInfo :: ParserInfo CommandLineWebdriverOptions
webDriverOptionsWithInfo = OA.info (commandLineWebdriverOptions mempty <**> helper)
  (
    fullDesc
    <> progDesc "Special options passed to the WebDriver formatter, if present.\n\nIf a flag is passed, it will override the value in the WdOptions configured in the code."
    <> header "WebDriver flags"
  )

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

  , optWebdriverOptions :: CommandLineWebdriverOptions
  , optSlackOptions :: CommandLineSlackOptions

  , optUserOptions :: a
  } deriving Show

data FullOptions a = RunOptions (CommandLineOptions a)
                   | ListTests
  deriving Show

mainCommandLineOptions :: Parser a -> Parser (CommandLineOptions a)
mainCommandLineOptions userOptionsParser = CommandLineOptions
  -- sandwich
  <$> formatter
  <*> logLevel
  <*> optional (strOption (long "filter" <> short 'f' <> help "Filter test tree by string matching text example labels" <> metavar "STRING"))
  <*> option auto (long "repeat" <> short 'r' <> showDefault <> help "Repeat the test N times and report how many failures occur" <> value 1 <> metavar "INT")
  <*> optional (strOption (long "fixed-root" <> help "Store test artifacts at a fixed path" <> metavar "STRING"))

  <*> optional (flag False True (long "list-tests" <> help "List individual test modules"))
  <*> optional (flag False True (long "print-slack-flags" <> help "Print the additional Slack flags"))
  <*> optional (flag False True (long "print-webdriver-flags" <> help "Print the additional Slack flags"))

  <*> commandLineWebdriverOptions internal
  <*> commandLineSlackOptions internal

  <*> userOptionsParser
  
formatter :: Parser FormatterType
formatter =
  flag' Print (long "print" <> help "Print to stdout")
  <|> flag' TUI (long "tui" <> help "Open terminal UI app")
  <|> flag' Silent (long "silent" <> help "Run silently (no main formatter)")
  <|> flag Auto Auto (long "auto" <> help "Automatically decide which formatter to use")

logLevel :: Parser (Maybe LogLevel)
logLevel =
  flag' (Just LevelDebug) (long "debug" <> help "Log level DEBUG")
  <|> flag' (Just LevelInfo) (long "info" <> help "Log level INFO")
  <|> flag' (Just LevelWarn) (long "warn" <> help "Log level WARN")
  <|> flag (Just LevelWarn) (Just LevelError) (long "error" <> help "Log level ERROR")

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

commandLineWebdriverOptions :: (forall f a. Mod f a) -> Parser CommandLineWebdriverOptions
commandLineWebdriverOptions maybeInternal = CommandLineWebdriverOptions
  <$> browserToUse maybeInternal
  <*> option auto (long "pool-size" <> short 'p' <> showDefault <> help "WebDriver pool size" <> value 4 <> metavar "INT" <> maybeInternal)
  <*> display maybeInternal
  <*> flag False True (long "fluxbox" <> help "Launch fluxbox as window manager when using Xvfb" <> maybeInternal)
  <*> flag False True (long "individual-videos" <> help "Record individual videos of each test (requires ffmpeg and Xvfb)" <> maybeInternal)
  <*> flag False True (long "error-videos" <> help "Record videos of each test but delete them unless there was an exception" <> maybeInternal)

browserToUse :: (forall f a. Mod f a) -> Parser BrowserToUse
browserToUse maybeInternal =
  flag' UseFirefox (long "firefox" <> help "Use Firefox" <> maybeInternal)
  <|> flag UseChrome UseChrome (long "chrome" <> help "Use Chrome (default)" <> maybeInternal)

display :: (forall f a. Mod f a) -> Parser DisplayType
display maybeInternal =
  flag' Current (long "current" <> help "Open browser in current display (default)" <> maybeInternal)
  <|> flag' Headless (long "headless" <> help "Open browser in headless mode" <> maybeInternal)
  <|> flag Current Xvfb (long "xvfb" <> help "Open browser in Xvfb session" <> maybeInternal)

-- * sandwich-slack options

data CommandLineSlackOptions = CommandLineSlackOptions {
  optSlackToken :: Maybe String
  , optSlackChannel :: Maybe String

  , optSlackTopMessage :: Maybe String

  , optSlackMaxFailures :: Maybe Int
  , optSlackMaxFailureReasonLines :: Maybe Int
  , optSlackMaxCallStackLines :: Maybe Int

  , optSlackVisibilityThreshold :: Maybe Int

  , optSlackMaxMessageSize :: Maybe Int
  } deriving Show

commandLineSlackOptions :: (forall f a. Mod f a) -> Parser CommandLineSlackOptions
commandLineSlackOptions maybeInternal = CommandLineSlackOptions
  <$> optional (strOption (long "slack-token" <> help "Slack token to use with the Slack formatter" <> metavar "STRING" <> maybeInternal))
  <*> optional (strOption (long "slack-channel" <> help "Slack channel to use with the Slack formatter" <> metavar "STRING" <> maybeInternal))

  <*> optional (strOption (long "slack-top-message" <> help "Top message to display on Slack progress bars" <> metavar "STRING" <> maybeInternal))

  <*> optional (option auto (long "slack-max-failures" <> help "Maximum number of failures to include in a message" <> metavar "INT" <> maybeInternal))
  <*> optional (option auto (long "slack-max-failure-reason-lines" <> help "Maximum number of lines for the failure reason underneath a failure" <> metavar "INT" <> maybeInternal))
  <*> optional (option auto (long "slack-max-callstack-lines" <> help "Maximum number of lines for the callstack reason underneath a failure" <> metavar "INT" <> maybeInternal))

  <*> optional (option auto (long "slack-visibility-threshold" <> help "Filter the headings on failures by visibility threshold" <> metavar "INT" <> maybeInternal))

  <*> optional (option auto (long "slack-max-message-size" <> help "Maximum message size in bytes (default: 8192)" <> metavar "INT" <> maybeInternal))

-- * Main parsing function

addOptionsFromArgs :: Options -> CommandLineOptions a -> IO (Options, Int)
addOptionsFromArgs baseOptions (CommandLineOptions {..}) = do
  let printFormatter = SomeFormatter $ defaultPrintFormatter { printFormatterLogLevel = optLogLevel }
  let tuiFormatter = SomeFormatter $ defaultTerminalUIFormatter { terminalUILogLevel = optLogLevel }

  maybeMainFormatter <- case (optRepeatCount, optFormatter) of
    (x, _) | x /= 1 -> return $ Just printFormatter
    (_, Auto) -> hIsTerminalDevice stdout >>= \case
      True -> return $ Just printFormatter
      False -> return $ Just tuiFormatter
    (_, TUI) -> return $ Just tuiFormatter
    (_, Print) -> return $ Just printFormatter
    (_, Silent) -> return Nothing

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
    , optionsFormatters = catMaybes [maybeMainFormatter, Just $ SomeFormatter defaultLogSaverFormatter]
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
