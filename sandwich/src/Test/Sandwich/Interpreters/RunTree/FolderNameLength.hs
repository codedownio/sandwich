module Test.Sandwich.Interpreters.RunTree.FolderNameLength (
  FolderComponent(..)
  , folderComponent
  , minFolderNameCap
  , computeFolderNameCaps
  , nameCapWithinDirectory
  ) where

import qualified Data.List as L
import qualified Data.Map as M
import Test.Sandwich.Interpreters.RunTree.Util


data FolderComponent = FolderComponent {
  folderComponentNodeId :: Int
  , folderComponentPaddingLength :: Int
  , folderComponentNameLength :: Int
  } deriving (Show, Eq)

folderComponent :: Int -> String -> Int -> Int -> FolderComponent
folderComponent nodeId name numSiblings indexInParent = FolderComponent nodeId paddingLen nameLen
  where
    paddingLen = L.length (folderNamePadding numSiblings indexInParent)
    nameLen = L.length (nodeToFolderName name numSiblings indexInParent) - paddingLen

minFolderNameCap :: Int
minFolderNameCap = 6

chainLength :: Maybe Int -> [FolderComponent] -> Int
chainLength cap chain = sum [1 + folderComponentPaddingLength c + maybe id min cap (folderComponentNameLength c) | c <- chain]

-- | Caps by node ID so every chain is at most @available@ long: each node gets the smallest of the largest uniform
-- caps fitting the chains through it. Also returns the chains that don't fit even at 'minFolderNameCap'.
computeFolderNameCaps :: Int -> [[FolderComponent]] -> (M.Map Int Int, [[FolderComponent]])
computeFolderNameCaps available chains = (caps, overflows)
  where
    caps = M.fromListWith min [(folderComponentNodeId c, cap) | chain <- chains, Just cap <- [chainCap available chain], c <- chain]
    overflows = [chain | chain <- chains, chainLength (Just minFolderNameCap) chain > available]

chainCap :: Int -> [FolderComponent] -> Maybe Int
chainCap available chain
  | fits Nothing = Nothing
  | otherwise = Just (go minFolderNameCap (L.maximum (minFolderNameCap : fmap folderComponentNameLength chain)))
  where
    fits cap = chainLength cap chain <= available

    go lo hi
      | hi - lo <= 1 = lo
      | fits (Just mid) = go mid hi
      | otherwise = go lo mid
      where mid = (lo + hi) `div` 2

-- | Name cap for a file directly in @dir@, followed by @suffixLength@ more characters, to stay within the budget.
nameCapWithinDirectory :: Maybe Int -> FilePath -> Int -> Int -> Int -> Maybe Int
nameCapWithinDirectory maybeBudget dir suffixLength numSiblings indexInParent = flip fmap maybeBudget $ \budget ->
  max minFolderNameCap (budget - L.length dir - 1 - L.length (folderNamePadding numSiblings indexInParent) - suffixLength)
