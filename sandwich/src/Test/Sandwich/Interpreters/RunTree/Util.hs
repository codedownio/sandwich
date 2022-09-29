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

-- | Append a log message outside of ExampleT. Only stored to in-memory logs, not disk.
-- Only for debugging the interpreter, should not be exposed.
appendLogMessage :: ToLogStr msg => TVar (Seq LogEntry) -> msg -> IO ()
appendLogMessage logs msg = do
  ts <- getCurrentTime
  atomically $ modifyTVar logs (|> LogEntry ts (Loc "" "" "" (0, 0) (0, 0)) "manual" LevelDebug (toLogStr msg))

getImmediateChildren :: Free (SpecCommand context m) () -> [Free (SpecCommand context m) ()]
getImmediateChildren (Free (It'' loc no l ex next)) = (Free (It'' loc no l ex (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Before'' loc no l f subspec next)) = (Free (Before'' loc no l f subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (After'' loc no l f subspec next)) = (Free (After'' loc no l f subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Introduce'' loc no l cl alloc cleanup subspec next)) = (Free (Introduce'' loc no l cl alloc cleanup subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (IntroduceWith'' loc no l cl action subspec next)) = (Free (IntroduceWith'' loc no l cl action subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Around'' loc no l f subspec next)) = (Free (Around'' loc no l f subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Describe'' loc no l subspec next)) = (Free (Describe'' loc no l subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Parallel'' loc no subspec next)) = (Free (Parallel'' loc no subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Pure ()) = [Pure ()]

countChildren :: Free (SpecCommand context m) () -> Int
countChildren = L.length . getImmediateChildren

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
