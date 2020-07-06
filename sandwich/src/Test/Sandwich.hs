{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.Sandwich (
  runSandwich
  , runSandwichTree
  , startSandwichTree

  , it
  , describe
  , introduce
  , introduceWith
  , before
  , beforeEach
  , after
  , afterEach
  , around
  , parallel

  , TopSpec

  , defaultOptions
  , optionsTestArtifactsDirectory
  , TestArtifactsDirectory(..)

  , BaseContext

  , Result(..)
  , FailureReason(..)

  , Label(..)

  , Spec, SpecWith, SpecFree, HasBaseContext, HasLabel, LabelValue(..), (:>)(..), ExampleM, ExampleT(..) -- Used in sandwich-webdriver

  , module Test.Sandwich.Contexts
  , module Test.Sandwich.Expectations
  , module Test.Sandwich.Logging

  ) where

import Control.Concurrent.Async
import qualified Control.Exception as E
import Control.Monad
import Data.String.Interpolate.IsString
import System.Directory
import System.FilePath
import System.Posix.Signals
import Test.Sandwich.Contexts
import Test.Sandwich.Expectations
import Test.Sandwich.Interpreters.RunTree
import Test.Sandwich.Logging
import Test.Sandwich.Shutdown
import Test.Sandwich.Types.Formatter
import Test.Sandwich.Types.Options
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


runSandwich :: (Formatter f) => Options -> f -> TopSpec -> IO ()
runSandwich options f spec = do
  rts <- startSandwichTree options spec

  formatterAsync <- async $ runFormatter f rts

  let shutdown = do
        putStrLn "Shutting down..."
        forM_ rts cancelRecursively
        -- cancel formatterAsync

  _ <- installHandler sigINT (Catch shutdown) Nothing

  putStrLn [i|Beginning wait for formatterAsync|]
  finalResult :: Either E.SomeException () <- E.try $ wait formatterAsync
  putStrLn [i|Final result: #{finalResult}|]

startSandwichTree :: Options -> TopSpec -> IO [RunTree]
startSandwichTree options@(Options {..}) spec = do
  runRoot <- case optionsTestArtifactsDirectory of
    TestArtifactsNone -> return Nothing
    TestArtifactsFixedDirectory dir -> do
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

  let baseContext = BaseContext {
        baseContextPath = mempty
        , baseContextOptions = options
        , baseContextRunRoot = runRoot
        }

  runTreeMain baseContext spec

runSandwichTree :: Options -> TopSpec -> IO [RunTree]
runSandwichTree options spec = do
  rts <- startSandwichTree options spec
  mapM_ (wait . runTreeAsync) rts
  return rts
