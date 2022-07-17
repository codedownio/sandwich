
{-| This module is based on hgold from hspec-golden-0.2.0.0, which is MIT licensed -}

module Test.Sandwich.Golden.Update where

import Control.Monad (forM_, when)
import Data.Monoid ((<>))
import Data.Version (showVersion)
import Options.Applicative
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, renameFile)


defaultDirGoldenTest :: FilePath
defaultDirGoldenTest = ".golden"

updateGolden :: FilePath -> IO ()
updateGolden dir = do
  putStrLn "Replacing golden with actual..."
  go dir
  putStrLn "Finish..."
 where
  go dir = do
    entries <- listDirectory dir
    forM_ entries $ \entry -> do
      let entryInDir = dir ++ "/" ++ entry
      isDir <- doesDirectoryExist entryInDir
      when isDir $ do
        mvActualToGolden entryInDir
        go entryInDir

mvActualToGolden :: FilePath -> IO ()
mvActualToGolden testPath =
  let actualFilePath = testPath ++ "/actual"
      goldenFilePath = testPath ++ "/golden"
   in do
     actualFileExist <- doesFileExist actualFilePath
     when actualFileExist (do
       putStrLn $ "  Replacing file: " ++ goldenFilePath ++ " with: " ++ actualFilePath
       renameFile actualFilePath goldenFilePath)
