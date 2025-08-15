
module Main where

import Control.Exception
import Data.Function
import qualified Data.List as L
import qualified Data.Map as M
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.Environment
import System.FilePath
import Test.Sandwich.TH


newtype SandwichDiscoverOptions = SandwichDiscoverOptions {
  sandwichDiscoverModulePrefix :: String -- TODO: don't use this, instead detect the module some other way (haskell-src-exts? fancy regex?)
  }

defaultSandwichDiscoverOptions :: SandwichDiscoverOptions
defaultSandwichDiscoverOptions = SandwichDiscoverOptions {
  sandwichDiscoverModulePrefix = ""
  }

tryParseArg :: SandwichDiscoverOptions -> String -> SandwichDiscoverOptions
tryParseArg options x
  | ("--module-prefix=" `L.isPrefixOf` x) = options { sandwichDiscoverModulePrefix = L.drop (L.length ("--module-prefix=" :: String)) x }
  | otherwise = options

main :: IO ()
main = do
  (originalFileName, inputFileName, outputFileName, SandwichDiscoverOptions {..}) <- getArgs >>= \case
    (w:x:y:remainingArgs) -> do
      let sandwichDiscoverOptions = L.foldl' tryParseArg defaultSandwichDiscoverOptions remainingArgs
      return (w, x, y, sandwichDiscoverOptions)
    xs -> throwIO $ userError ([i|sandwich-discover: expected 3+ args but got '#{xs}'|])

  let baseDir' = dropExtension originalFileName
  doesDirectoryExist baseDir' >>= \case
    False -> throwIO $ userError ([i|sandwich-discover: expected directory to exist (#{baseDir'})|])
    True -> return ()
  baseDir <- canonicalizePath baseDir'

  -- Build a map from imported name (possibly dedupped by adding numbers) to full module name
  moduleMap <- buildModuleMap baseDir sandwichDiscoverModulePrefix

  let testImports = [[i|import qualified #{y} as #{x}|] | (x, y) <- M.toList moduleMap]
  let testImportsList = [[i|#{x}.tests|] | (x, _) <- M.toList moduleMap]

  contents <- T.readFile inputFileName
  let finalContents = contents
        & T.replace "#insert_test_imports" (T.unlines testImports)
        & T.replace "#test_imports_list" ("[" <> T.intercalate ", " testImportsList <> "]")

  T.writeFile outputFileName finalContents
