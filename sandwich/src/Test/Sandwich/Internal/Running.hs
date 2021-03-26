{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiWayIf #-}

module Test.Sandwich.Internal.Running where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Free
import Control.Monad.State
import Data.Char
import Data.Function
import qualified Data.List as L
import Data.Maybe
import Data.String.Interpolate
import qualified Data.Text as T
import System.Directory
import System.Exit
import System.FilePath
import Test.Sandwich.Interpreters.FilterTree
import Test.Sandwich.Interpreters.RunTree
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.Interpreters.StartTree
import Test.Sandwich.Options
import Test.Sandwich.TestTimer
import Test.Sandwich.Types.ArgParsing
import Test.Sandwich.Types.General
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Test.Sandwich.Types.TestTimer
import Test.Sandwich.Util


startSandwichTree :: Maybe (CommandLineOptions ()) -> Options -> TopSpec -> IO [RunNode BaseContext]
startSandwichTree maybeCommandLineOptions options spec = do
  baseContext <- baseContextFromOptions maybeCommandLineOptions options
  startSandwichTree' baseContext options spec

startSandwichTree' :: BaseContext -> Options -> TopSpec -> IO [RunNode BaseContext]
startSandwichTree' baseContext (Options {..}) spec' = do
  let spec = case optionsFilterTree of
        Nothing -> spec'
        Just (TreeFilter match) -> filterTree match spec'

  runTree <- atomically $ specToRunTreeVariable baseContext spec

  unless optionsDryRun $ do
    void $ async $ void $ runNodesSequentially runTree baseContext

  return runTree

runSandwichTree :: Maybe (CommandLineOptions ()) -> Options -> TopSpec -> IO [RunNode BaseContext]
runSandwichTree maybeCommandLineOptions options spec = do
  rts <- startSandwichTree maybeCommandLineOptions options spec
  mapM_ waitForTree rts
  return rts

-- | For 0 repeats, repeat until a failure
runWithRepeat :: Int -> IO (ExitReason, Int) -> IO ()
runWithRepeat 0 action = do
  (_, numFailures) <- action
  if | numFailures == 0 -> runWithRepeat 0 action
     | otherwise -> exitFailure
-- | For 1 repeat, run once and return
runWithRepeat n action = do
  (successes, total) <- (flip execStateT (0 :: Int, 0 :: Int)) $ flip fix (n - 1) $ \loop n -> do
    (exitReason, numFailures) <- liftIO action

    modify $ \(successes, total) -> (successes + (if numFailures == 0 then 1 else 0), total + 1)

    if | exitReason == InterruptExit -> return ()
       | n > 0 -> loop (n - 1)
       | otherwise -> return ()

  putStrLn [i|#{successes} runs succeeded out of #{total} repeats|]

  when (successes /= total) $ exitFailure

baseContextFromOptions :: Maybe (CommandLineOptions ()) -> Options -> IO BaseContext
baseContextFromOptions maybeCommandLineOptions options@(Options {..}) = do
  runRoot <- case optionsTestArtifactsDirectory of
    TestArtifactsNone -> return Nothing
    TestArtifactsFixedDirectory dir' -> do
      dir <- case isAbsolute dir' of
        True -> return dir'
        False -> do
          here <- getCurrentDirectory
          return $ here </> dir'

      createDirectoryIfMissing True dir
      return $ Just dir
    TestArtifactsGeneratedDirectory base' f -> do
      base <- case isAbsolute base' of
        True -> return base'
        False -> do
          here <- getCurrentDirectory
          return $ here </> base'

      name <- f
      let dir = base </> name
      createDirectoryIfMissing True dir
      return $ Just dir

  testTimer <- case (optionsTestTimerType, runRoot) of
    (SpeedScopeTestTimerType {..}, Just rr) -> liftIO $ newSpeedScopeTestTimer rr speedScopeTestTimerWriteRawTimings
    _ -> return NullTestTimer

  let errorSymlinksDir = (</> "errors") <$> runRoot
  whenJust errorSymlinksDir $ createDirectoryIfMissing True
  return $ BaseContext {
    baseContextPath = mempty
    , baseContextOptions = options
    , baseContextRunRoot = runRoot
    , baseContextErrorSymlinksDir = errorSymlinksDir
    , baseContextOnlyRunIds = Nothing
    , baseContextTestTimerProfile = defaultProfileName
    , baseContextTestTimer = testTimer
    , baseContextCommandLineOptions = maybeCommandLineOptions
    }


-- | Gather all node options from a spec
gatherNodeOptions :: Free (SpecCommand context m) r -> [NodeOptions]
gatherNodeOptions (Free x@(It'' {})) = (nodeOptions x) : gatherNodeOptions (next x)
gatherNodeOptions (Free (IntroduceWith'' {..})) = nodeOptions : (gatherNodeOptions next <> gatherNodeOptions subspecAugmented)
gatherNodeOptions (Free (Introduce'' {..})) = nodeOptions : (gatherNodeOptions next <> gatherNodeOptions subspecAugmented)
gatherNodeOptions (Free x) = (nodeOptions x) : (gatherNodeOptions (next x) <> gatherNodeOptions (subspec x))
gatherNodeOptions (Pure _) = []

gatherMainFunctions :: Free (SpecCommand context m) r -> [NodeModuleInfo]
gatherMainFunctions tests = gatherNodeOptions tests
                            & fmap nodeOptionsModuleInfo
                            & catMaybes


takenMainOptions = []

gatherShorthands :: [NodeModuleInfo] -> [(NodeModuleInfo, T.Text)]
gatherShorthands = gatherShorthands' []
  where
    gatherShorthands' :: [T.Text] -> [NodeModuleInfo] -> [(NodeModuleInfo, T.Text)]
    gatherShorthands' _ [] = []
    gatherShorthands' taken (x:xs) = (x, newShorthand) : (gatherShorthands' (newShorthand : taken) xs)
      where newShorthand = getShorthand taken x

    getShorthand :: [T.Text] -> NodeModuleInfo -> T.Text
    getShorthand taken nmi = head $ filter (\x -> x `notElem` taken && x `notElem` takenMainOptions) $ getCandidates nmi

    getCandidates :: NodeModuleInfo -> [T.Text]
    getCandidates (NodeModuleInfo {nodeModuleInfoModuleName=modName}) = candidates
      where parts = T.splitOn "." (T.pack modName)
            lastPart = last parts
            candidates = (toDashed lastPart) : [toDashed [i|#{lastPart}#{n}|] | n <- [(2 :: Integer)..]]

    toDashed :: T.Text -> T.Text
    toDashed t = t & T.unpack
                   & splitR isUpper
                   & fmap (T.toLower . T.pack)
                   & T.intercalate "-"

    splitR :: (Char -> Bool) -> String -> [String]
    splitR _ [] = []
    splitR p s =
      let
        go :: Char -> String -> [String]
        go m s' = case L.break p s' of
          (b', [])     -> [ m:b' ]
          (b', x:xs) -> ( m:b' ) : go x xs
      in case L.break p s of
        (b,  [])    -> [ b ]
        ([], h:t) -> go h t
        (b,  h:t) -> b : go h t
