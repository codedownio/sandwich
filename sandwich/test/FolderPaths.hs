{-# LANGUAGE DataKinds #-}

module FolderPaths where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer
import qualified Data.List as L
import qualified Data.Map as M
import Data.Maybe
import Data.String.Interpolate
import GHC.Stack
import System.FilePath
import Test.Sandwich
import Test.Sandwich.Internal
import TestUtil
import UnliftIO.Exception


tests :: MonadIO m => WriterT [SomeException] m ()
tests = do
  run capsMakeEveryChainFit
  run capsLeaveShallowChainsAlone
  run capsReportOverflows
  run deepTreeFitsBudget
  run deepTreeKeepsShallowNamesAndUniqueSiblings
  run noBudgetLeavesNamesAlone
  run tinyBudgetWarns

main :: IO ()
main = mainWith tests

-- * Cap computation

-- | Room left under a 200 budget after a 69-char root.
available :: Int
available = 200 - 69

chainLengthWithCaps :: M.Map Int Int -> [FolderComponent] -> Int
chainLengthWithCaps caps chain =
  sum [1 + folderComponentPaddingLength c + maybe id min (M.lookup (folderComponentNodeId c) caps) (folderComponentNameLength c) | c <- chain]

-- Node 0 is the shared root, 1..10 form a deep chain of long names, and 11 is a short shallow sibling.
deepChain :: [FolderComponent]
deepChain = [FolderComponent n 2 80 | n <- [0..10]]

shallowChain :: [FolderComponent]
shallowChain = [FolderComponent 0 2 80, FolderComponent 11 2 20]

allChains :: [[FolderComponent]]
allChains = shallowChain : [L.take n deepChain | n <- [1..L.length deepChain]]

capsMakeEveryChainFit :: (HasCallStack) => IO ()
capsMakeEveryChainFit = do
  let (caps, overflows) = computeFolderNameCaps available allChains
  overflows `mustBe` []
  forM_ allChains $ \chain -> do
    let len = chainLengthWithCaps caps chain
    unless (len <= available) $ expectationFailure [i|Chain of length #{len} doesn't fit: #{chain}|]

capsLeaveShallowChainsAlone :: (HasCallStack) => IO ()
capsLeaveShallowChainsAlone = do
  let (caps, _) = computeFolderNameCaps available allChains
  M.lookup 11 caps `mustBe` Nothing
  forM_ [0..10] $ \n -> unless (isJust (M.lookup n caps)) $ expectationFailure [i|Expected node #{n} to be capped|]

capsReportOverflows :: (HasCallStack) => IO ()
capsReportOverflows = do
  let chain = [FolderComponent n 2 80 | n <- [0..40]]
  let (caps, overflows) = computeFolderNameCaps available [chain]
  overflows `mustBe` [chain]
  M.lookup 0 caps `mustBe` Just minFolderNameCap

-- * Spec trees

longLabel :: Int -> String
longLabel n = [i|#{n}_manually goes back to the revision prior to the one that created the dummy settings file|]

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

longRoot :: FilePath
longRoot = "C:\\Users\\tom\\AppData\\Local\\codedown-dev\\tests\\r\\261112093000"

getFolders :: Options -> CoreSpec -> IO ([(String, FilePath)], [String])
getFolders options spec = do
  bc <- baseContextFromOptions options
  let (tree, warnings) = specToRunTreeWithWarnings (bc { baseContextRunRoot = Just longRoot, baseContextOptions = options }) spec
  let folders = [(label, folder) | (label, Just folder) <- concatMap (extractValues (\n -> (runTreeLabel (runNodeCommon n), runTreeFolder (runNodeCommon n)))) tree]
  return (folders, warnings)

budgetOptions :: Options
budgetOptions = defaultOptions { optionsMaxFolderPathLength = Just 200 }

deepTreeFitsBudget :: (HasCallStack) => IO ()
deepTreeFitsBudget = do
  (folders, warnings) <- getFolders budgetOptions deepSpec
  warnings `mustBe` []
  forM_ folders $ \(_, folder) ->
    unless (L.length folder <= 200) $ expectationFailure [i|Folder too long (#{L.length folder}): #{folder}|]
  -- Sanity check that the budget actually did something
  (uncapped, _) <- getFolders (budgetOptions { optionsMaxFolderPathLength = Nothing }) deepSpec
  unless (L.any (\(_, f) -> L.length f > 200) uncapped) $ expectationFailure "Expected the uncapped tree to exceed the budget"

deepTreeKeepsShallowNamesAndUniqueSiblings :: (HasCallStack) => IO ()
deepTreeKeepsShallowNamesAndUniqueSiblings = do
  (folders, _) <- getFolders budgetOptions deepSpec
  let paths = fmap snd folders
  L.length (L.nub paths) `mustBe` L.length paths
  case L.lookup "does a shallow thing with a fairly long but harmless label" folders of
    Nothing -> expectationFailure "Couldn't find shallow node"
    Just f -> takeFileName f `mustBe` "does a shallow thing with a fairly long but harmless label"

noBudgetLeavesNamesAlone :: (HasCallStack) => IO ()
noBudgetLeavesNamesAlone = do
  (folders, warnings) <- getFolders (budgetOptions { optionsMaxFolderPathLength = Nothing }) deepSpec
  warnings `mustBe` []
  case L.lookup (longLabel 1) folders of
    Nothing -> expectationFailure "Couldn't find deep node"
    Just f -> takeFileName f `mustBe` ("1_" <> longLabel 1)

tinyBudgetWarns :: (HasCallStack) => IO ()
tinyBudgetWarns = do
  (folders, warnings) <- getFolders (budgetOptions { optionsMaxFolderPathLength = Just 150 }) deepSpec
  L.length warnings `mustBe` 1
  -- Still uses the shortest names rather than failing
  let paths = fmap snd folders
  L.length (L.nub paths) `mustBe` L.length paths
