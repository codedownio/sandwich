{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

{-| This module is based on hgold from hspec-golden-0.2.0.0, which is MIT licensed -}

module Test.Sandwich.Golden.Update where

import Control.Monad
import Data.Maybe
import Data.String.Interpolate
import System.Directory


defaultDirGoldenTest :: FilePath
defaultDirGoldenTest = ".golden"

updateGolden :: Maybe FilePath -> IO ()
updateGolden (fromMaybe defaultDirGoldenTest -> dir) = do
  putStrLn "Replacing golden with actual..."
  go dir
  putStrLn "Done!"
  where
    go dir = listDirectory dir >>= mapM_ processEntry

    processEntry (((dir ++ "/") ++) -> entryInDir) = do
      isDir <- doesDirectoryExist entryInDir
      when isDir $ do
        mvActualToGolden entryInDir
        go entryInDir

mvActualToGolden :: FilePath -> IO ()
mvActualToGolden testPath = do
  let actualFilePath = testPath ++ "/actual"
  let goldenFilePath = testPath ++ "/golden"

  exists <- doesFileExist actualFilePath
  when exists $ do
    putStrLn [i|  #{goldenFilePath} <-- #{actualFilePath}|]
    renameFile actualFilePath goldenFilePath
