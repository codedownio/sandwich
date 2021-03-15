{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Exception
import Control.Monad
import Data.Function
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe (isNothing)
import Data.String.Interpolate
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Directory
import System.Environment
import System.FilePath


data SandwichDiscoverOptions = SandwichDiscoverOptions {
  sandwichDiscoverModulePrefix :: String
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

  -- Build a map from module name (possibly dedupped by adding numbers) to qualified path
  moduleMap <- buildModuleMap baseDir

  let testImports = [[i|import qualified #{sandwichDiscoverModulePrefix}#{y} as #{x}|] | (x, y) <- M.toList moduleMap]
  let testImportsList = [[i|#{x}.tests|] | (x, _) <- M.toList moduleMap]

  contents <- T.readFile inputFileName
  let finalContents = contents
        & T.replace "#insert_test_imports" (T.unlines testImports)
        & T.replace "#test_imports_list" ("[" <> T.intercalate ", " testImportsList <> "]")

  T.writeFile outputFileName finalContents

type ModuleMap = M.Map String String

buildModuleMap :: FilePath -> IO ModuleMap
buildModuleMap baseDir = traverseDir (const True) (\x y -> return $ addModuleToMap baseDir x y) mempty baseDir

addModuleToMap :: FilePath -> ModuleMap -> FilePath -> ModuleMap
addModuleToMap relativeTo mm path@(takeExtension -> ".hs") = case pathParts of
  [] -> mm
  _ -> M.insert moduleName (L.intercalate "." pathParts) mm
  where
    relativePath = (takeFileName relativeTo) </> (makeRelative relativeTo path)
    pathParts = splitDirectories $ dropExtension relativePath
    baseModuleName = last pathParts
    moduleName = head $ filter doesNotExist (baseModuleName : [baseModuleName <> show n | n <- [1..]])
    doesNotExist x = isNothing (M.lookup x mm)
addModuleToMap _ mm _ = mm

-- | From https://stackoverflow.com/a/51713361/2659595
traverseDir :: (FilePath -> Bool) -> (b -> FilePath -> IO b) -> b -> FilePath -> IO b
traverseDir validDir transition =
  let go state dirPath = do
        names <- listDirectory dirPath
        let paths = map (dirPath </>) names
        (dirPaths, filePaths) <- partitionM doesDirectoryExist paths
        state' <- foldM transition state filePaths -- process current dir
        foldM go state' (filter validDir dirPaths) -- process subdirs
 in go

-- | From https://hackage.haskell.org/package/extra-1.7.9/docs/src/Control.Monad.Extra.html#partitionM
partitionM :: Monad m => (a -> m Bool) -> [a] -> m ([a], [a])
partitionM _ [] = pure ([], [])
partitionM f (x:xs) = do
    res <- f x
    (as,bs) <- partitionM f xs
    pure ([x | res]++as, [x | not res]++bs)
