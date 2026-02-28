{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE CPP #-}

module Test.Sandwich.Interpreters.RunTree.Util where

import Control.Concurrent.STM
import Control.Monad.Free
import Control.Monad.Logger
import qualified Data.List as L
import Data.Sequence as Seq hiding ((:>))
import Data.String.Interpolate
import Data.Time.Clock
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
nodeToFolderName name 1 0 = truncateFileNameToLength maxFileNameLength $ fixupName name
nodeToFolderName name numSiblings indexInParent = padding <> truncateFileNameToLength (maxFileNameLength - (L.length padding)) (fixupName name)
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

    padding = if | numSiblings == 1 -> ""
                 | otherwise -> paddedNumber <> "_"


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
