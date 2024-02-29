
{-| This module is based on hgold from hspec-golden-0.2.0.0, which is MIT licensed -}

module Test.Sandwich.Golden.Update (
  updateGolden
  , defaultDirGoldenTest
  ) where

import Control.Exception.Safe
import Control.Monad
import Data.Maybe
import Data.String.Interpolate
import System.Console.ANSI
import System.Directory
import System.Environment


defaultDirGoldenTest :: FilePath
defaultDirGoldenTest = ".golden"

updateGolden :: Maybe FilePath -> IO ()
updateGolden (fromMaybe defaultDirGoldenTest -> dir) = do
  enableColor <- lookupEnv "NO_COLOR" >>= \case
    Nothing -> return EnableColor
    Just _ -> return DisableColor

  putStrLnColor enableColor green "Replacing golden with actual..."
  go enableColor dir
  putStrLnColor enableColor green "Done!"

  where
    go enableColor dir' = listDirectory dir' >>= mapM_ (processEntry enableColor)

    processEntry enableColor (((dir ++ "/") ++) -> entryInDir) = do
      isDir <- doesDirectoryExist entryInDir
      when isDir $ do
        mvActualToGolden enableColor entryInDir
        go enableColor entryInDir

mvActualToGolden :: EnableColor -> FilePath -> IO ()
mvActualToGolden enableColor testPath = do
  let actualFilePath = testPath ++ "/actual"
  let goldenFilePath = testPath ++ "/golden"

  exists <- doesFileExist actualFilePath
  when exists $ do
    putStr [i|  #{goldenFilePath}|]
    putStrColor enableColor magenta " <-- "
    putStrLnColor enableColor red [i|#{actualFilePath}|]

    renameFile actualFilePath goldenFilePath

green, red, magenta :: SGR
green = SetColor Foreground Dull Green
red = SetColor Foreground Dull Red
magenta = SetColor Foreground Dull Magenta

putStrColor :: EnableColor -> SGR -> String -> IO ()
putStrColor EnableColor color s = bracket_ (setSGR [color]) (setSGR [Reset]) (putStr s)
putStrColor DisableColor _ s = putStr s

putStrLnColor :: EnableColor -> SGR -> String -> IO ()
putStrLnColor EnableColor color s = bracket_ (setSGR [color]) (setSGR [Reset]) (putStrLn s)
putStrLnColor DisableColor _ s = putStrLn s

data EnableColor = EnableColor | DisableColor
