{-# LANGUAGE DataKinds #-}

module FolderPaths where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import qualified Data.List as L
import Data.String.Interpolate
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Internal
import TestUtil
import UnliftIO.Exception


tests :: MonadIO m => WriterT [SomeException] m ()
tests = do
  -- Deep tree fits the max length
  run $ do
    folders <- getFolders (Just 150)
    forM_ folders $ \(_, folder) ->
      unless (L.length folder <= 150) $ expectationFailure [i|Folder too long (#{L.length folder}): #{folder}|]
    uncapped <- getFolders Nothing
    unless (L.any (\(_, f) -> L.length f > 150) uncapped) $ expectationFailure "Expected the uncapped tree to exceed the max length"

  -- Shallow names are kept
  run $ do
    folders <- getFolders (Just 150)
    case L.lookup "does a shallow thing with a fairly long but harmless label" folders of
      Nothing -> expectationFailure "Couldn't find shallow node"
      Just f -> takeFileName f `mustBe` "does a shallow thing with a fairly long but harmless label"

  -- Siblings stay unique
  run $ do
    paths <- fmap snd <$> getFolders (Just 150)
    L.length (L.nub paths) `mustBe` L.length paths

  -- No max length leaves names alone
  run $ do
    folders <- getFolders Nothing
    case L.lookup (longLabel 1) folders of
      Nothing -> expectationFailure "Couldn't find deep node"
      Just f -> takeFileName f `mustBe` ("1_" <> longLabel 1)

  -- A tiny max length uses the shortest names
  run $ do
    paths <- fmap snd <$> getFolders (Just 50)
    L.length (L.nub paths) `mustBe` L.length paths
    let deepest = L.maximumBy (\a b -> compare (L.length (splitDirectories a)) (L.length (splitDirectories b))) paths
    forM_ (splitDirectories (makeRelative "/root/results" deepest)) $ \component ->
      unless (L.length component <= 9) $ expectationFailure [i|Expected the shortest names, got: #{deepest}|]

-- | Nested describes with long labels, each with a couple of its, plus a shallow sibling at the top.
deepSpec :: CoreSpec
deepSpec = do
  describe "shallow sibling with a fairly long but harmless label" $
    it "does a shallow thing with a fairly long but harmless label" $ return ()
  go (12 :: Int)
  where
    go 0 = it (longLabel 0) $ return ()
    go n = describe (longLabel n) $ do
      it [i|first #{longLabel n}|] $ return ()
      go (n - 1)
      it [i|last #{longLabel n}|] $ return ()

longLabel :: Int -> String
longLabel n = [i|#{n}_manually goes back to the revision prior to the one that created the dummy settings file|]

-- | Folders by label, using 'getRunTree' (which puts the run root at "/root").
getFolders :: Maybe Int -> IO [(String, FilePath)]
getFolders maxLength = do
  tree <- getRunTree (defaultOptions { optionsMaxFolderPathLength = maxLength }) deepSpec
  return [(label, folder) | (label, Just folder) <- concatMap (extractValues (\n -> (runTreeLabel (runNodeCommon n), runTreeFolder (runNodeCommon n)))) tree]

main :: IO ()
main = mainWith tests
