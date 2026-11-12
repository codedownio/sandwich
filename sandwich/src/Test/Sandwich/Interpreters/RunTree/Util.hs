{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE CPP #-}

module Test.Sandwich.Interpreters.RunTree.Util where

import Control.Concurrent.STM
import Control.Monad.Free
import qualified Data.List as L
import qualified Data.Map as M
import Data.String.Interpolate
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Text.Printf


-- | Wait for a tree, catching any synchronous exceptions and returning them as a list
waitForTree :: RunNode context -> IO Result
waitForTree node = atomically $
  readTVar (runTreeStatus $ runNodeCommon node) >>= \case
    Done {statusResult} -> return statusResult
    NotStarted {} -> retry
    Running {} -> retry

-- | Count how many folder children are present as children or siblings of the given node.
countImmediateFolderChildren :: Free (SpecCommand context m) a -> Int
countImmediateFolderChildren (Free (It'' _loc no _l _ex next))
  | nodeOptionsCreateFolder no = 1 + countImmediateFolderChildren next
  | otherwise = countImmediateFolderChildren next
countImmediateFolderChildren (Free (Introduce'' _loc no _l _cl _alloc _cleanup subspec next))
  | nodeOptionsCreateFolder no = 1 + countImmediateFolderChildren next
  | otherwise = countImmediateFolderChildren subspec + countImmediateFolderChildren next
countImmediateFolderChildren (Free (IntroduceWith'' _loc no _l _cl _action subspec next))
  | nodeOptionsCreateFolder no = 1 + countImmediateFolderChildren next
  | otherwise = countImmediateFolderChildren subspec + countImmediateFolderChildren next
countImmediateFolderChildren (Free node)
  | nodeOptionsCreateFolder (nodeOptions node) = 1 + countImmediateFolderChildren (next node)
  | otherwise = countImmediateFolderChildren (subspec node) + countImmediateFolderChildren (next node)
countImmediateFolderChildren (Pure _) = 0

-- | Count how many folder children are present as children of the given node.
countSubspecFolderChildren :: Free (SpecCommand context m) a -> Int
countSubspecFolderChildren (Free (It'' {})) = 0
countSubspecFolderChildren (Free (Introduce'' {subspecAugmented})) = countImmediateFolderChildren subspecAugmented
countSubspecFolderChildren (Free (IntroduceWith'' {subspecAugmented})) = countImmediateFolderChildren subspecAugmented
countSubspecFolderChildren (Free node) = countImmediateFolderChildren (subspec node)
countSubspecFolderChildren (Pure _) = 0

maxFileNameLength :: Int
maxFileNameLength = 150

nodeToFolderName :: String -> Int -> Int -> String
nodeToFolderName = nodeToFolderNameCapped Nothing

-- | Like 'nodeToFolderName', but with the name part (everything after the index padding) capped at the
-- given length. The padding is never truncated, so siblings stay unique however hard the name is capped.
nodeToFolderNameCapped :: Maybe Int -> String -> Int -> Int -> String
nodeToFolderNameCapped cap name numSiblings indexInParent = padding <> truncateFileNameToLength nameCap (fixupName name)
  where
    padding = folderNamePadding numSiblings indexInParent
    nameCap = maybe id min cap (maxFileNameLength - L.length padding)

-- | The index prefix that keeps sibling folder names unique (empty for an only child).
folderNamePadding :: Int -> Int -> String
folderNamePadding 1 _ = ""
folderNamePadding numSiblings indexInParent = paddedNumber <> "_"
  where
    paddingNeeded
      | numSiblings < 10 = 1
      | numSiblings < 100 = 2
      | numSiblings < 1000 = 3
      | numSiblings < 10000 = 4
      | numSiblings < 100000 = 5
      | numSiblings < 1000000 = 6
      | numSiblings < 10000000 = 7
      | numSiblings < 100000000 = 8
      | otherwise = 15

    paddedNumber = printf [i|%0#{paddingNeeded :: Int}d|] indexInParent

-- * Fitting folder paths within a length budget

-- | One component of a node folder path.
data FolderComponent = FolderComponent {
  folderComponentNodeId :: Int
  -- ^ The node that owns this folder.
  , folderComponentPaddingLength :: Int
  -- ^ Length of the index padding, which is never truncated.
  , folderComponentNameLength :: Int
  -- ^ Length of the name part when uncapped.
  } deriving (Show, Eq)

folderComponent :: Int -> String -> Int -> Int -> FolderComponent
folderComponent nodeId name numSiblings indexInParent = FolderComponent nodeId paddingLen nameLen
  where
    paddingLen = L.length (folderNamePadding numSiblings indexInParent)
    nameLen = L.length (nodeToFolderNameCapped Nothing name numSiblings indexInParent) - paddingLen

-- | The shortest name part we'll cap to: "..." plus a few characters of the name.
minFolderNameCap :: Int
minFolderNameCap = 6

-- | A path from the run root to a node folder, along with the number of characters to reserve for whatever
-- gets written inside that folder.
data FolderChain = FolderChain {
  folderChainReserve :: Int
  , folderChainComponents :: [FolderComponent]
  -- ^ Ordered from the root down to the node.
  } deriving (Show, Eq)

-- | Length of the path @root </> c1 </> ... </> cn@ plus the reserve, with every name part capped at the given length.
folderChainLength :: Int -> Maybe Int -> FolderChain -> Int
folderChainLength rootLen cap (FolderChain reserve components) =
  rootLen + reserve + sum [1 + folderComponentPaddingLength c + maybe id min cap (folderComponentNameLength c) | c <- components]

-- | The largest cap that fits the chain strictly under the budget. 'Nothing' means no cap is needed.
-- 'Left' means even 'minFolderNameCap' doesn't fit.
folderChainCap :: Int -> Int -> FolderChain -> Either Int (Maybe Int)
folderChainCap budget rootLen chain
  | fits Nothing = Right Nothing
  | not (fits (Just minFolderNameCap)) = Left minFolderNameCap
  | otherwise = Right (Just (go minFolderNameCap maxNameLen))
  where
    fits cap = folderChainLength rootLen cap chain < budget
    maxNameLen = L.maximum (minFolderNameCap : fmap folderComponentNameLength (folderChainComponents chain))

    -- Invariant: lo fits, hi doesn't
    go lo hi
      | hi - lo <= 1 = lo
      | fits (Just mid) = go mid hi
      | otherwise = go lo mid
      where mid = (lo + hi) `div` 2

-- | Choose a cap for each node folder so that every chain fits within the budget.
--
-- Each chain gets the largest uniform cap that fits it, and each node takes the smallest cap of any chain passing
-- through it. Every chain then fits, since none of its nodes is capped looser than the chain's own cap, while
-- subtrees without deep chains keep their full names.
--
-- Returns the caps by node ID, plus the chains that don't fit even at 'minFolderNameCap'.
computeFolderNameCaps :: Int -> Int -> [FolderChain] -> (M.Map Int Int, [FolderChain])
computeFolderNameCaps budget rootLen chains = (caps, overflows)
  where
    results = [(chain, folderChainCap budget rootLen chain) | chain <- chains]

    overflows = [chain | (chain, Left _) <- results]

    caps = M.fromListWith min [(folderComponentNodeId c, cap)
                              | (chain, result) <- results
                              , Just cap <- [either Just id result]
                              , c <- folderChainComponents chain]

-- | The name cap to use for a file placed directly in @dir@ (followed by @suffixLength@ more characters, like an
-- extension), so its full path stays under the budget. 'Nothing' if there's no budget.
nameCapWithinDirectory :: Maybe Int -> FilePath -> Int -> Int -> Int -> Maybe Int
nameCapWithinDirectory maybeBudget dir suffixLength numSiblings indexInParent = flip fmap maybeBudget $ \budget ->
  max minFolderNameCap (budget - 1 - L.length dir - 1 - L.length (folderNamePadding numSiblings indexInParent) - suffixLength)


charsToReplace :: [Char]
#ifdef mingw32_HOST_OS
charsToReplace = ['\\', '/', ':', '*', '?', '"', '<', '>', '|']
#else
charsToReplace = ['/']
#endif

fixupName :: String -> String
fixupName = replaceAll charsToReplace '_'
  where
    replaceAll :: Eq a => [a] -> a -> [a] -> [a]
    replaceAll from to = map $ \c -> if c `L.elem` from then to else c

truncateFileNameToLength :: Int -> String -> String
truncateFileNameToLength len x | L.length x <= len = x
truncateFileNameToLength len x = "..." <> (takeEnd (len - 3) x)

takeEnd :: Int -> [a] -> [a]
takeEnd j xs = f xs (L.drop j xs)
  where f (_:zs) (_:ys) = f zs ys
        f zs _ = zs
