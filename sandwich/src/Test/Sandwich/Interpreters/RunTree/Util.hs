{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
-- |

module Test.Sandwich.Interpreters.RunTree.Util where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception.Safe
import Control.Monad.Free
import Control.Monad.Logger
import qualified Data.List as L
import Data.Sequence as Seq hiding ((:>))
import Data.String.Interpolate
import Data.Time.Clock
import System.FilePath
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import Text.Printf

-- | Wait for a tree, catching any synchronous exceptions
waitForTree :: [RunTree] -> IO (Either SomeException ())
waitForTree rts = tryAny (mapM_ wait (fmap runTreeAsync rts))

-- | Append a log message outside of ExampleT. Only stored to in-memory logs, not disk.
-- Only for debugging the interpreter, should not be exposed.
appendLogMessage :: ToLogStr msg => TVar (Seq LogEntry) -> msg -> IO ()
appendLogMessage logs msg = do
  ts <- getCurrentTime
  atomically $ modifyTVar logs (|> LogEntry ts (Loc "" "" "" (0, 0) (0, 0)) "manual" LevelDebug (toLogStr msg))

getImmediateChildren :: Free (SpecCommand context) () -> [Free (SpecCommand context) ()]
getImmediateChildren (Free (It l ex next)) = (Free (It l ex (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Before l f subspec next)) = (Free (Before l f subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (After l f subspec next)) = (Free (After l f subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Introduce l cl alloc cleanup subspec next)) = (Free (Introduce l cl alloc cleanup subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (IntroduceWith l cl action subspec next)) = (Free (IntroduceWith l cl action subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Around l f subspec next)) = (Free (Around l f subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Describe l subspec next)) = (Free (Describe l subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Free (Parallel subspec next)) = (Free (Parallel subspec (Pure ()))) : getImmediateChildren next
getImmediateChildren (Pure ()) = [Pure ()]

countChildren :: Free (SpecCommand context) () -> Int
countChildren = L.length . getImmediateChildren

appendFolder :: RunTreeContext context -> String -> Maybe FilePath
appendFolder (RunTreeContext {runTreeCurrentFolder=Nothing}) _ = Nothing
appendFolder (RunTreeContext {runTreeCurrentFolder=(Just f), ..}) l = Just (f </> (nodeToFolderName l runTreeNumSiblings runTreeIndexInParent))

nodeToFolderName :: String -> Int -> Int -> String
nodeToFolderName name numSiblings indexInParent = padding <> fixupName name
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

    paddedNumber = printf [i|%0#{paddingNeeded}d|] indexInParent

    padding = if | numSiblings == 1 -> ""
                 | otherwise -> paddedNumber <> "_"

    fixupName = replace '/' '_'

    replace :: Eq a => a -> a -> [a] -> [a]
    replace a b = map $ \c -> if c == a then b else c
