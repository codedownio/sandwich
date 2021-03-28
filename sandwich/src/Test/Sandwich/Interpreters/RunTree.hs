{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Sandwich.Interpreters.RunTree (
  specToRunTree
  , specToRunTreeVariable
  , isEmptySpec
  ) where

import Control.Concurrent.STM
import Control.Monad.Free
import Control.Monad.Trans.RWS
import Data.Functor.Identity
import qualified Data.List as L
import Data.Sequence
import GHC.Stack
import System.FilePath
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.RunTree
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


specToRunTree :: BaseContext -> Free (SpecCommand BaseContext IO) () -> [RunNodeFixed BaseContext]
specToRunTree baseContext spec = runIdentity $ specToRunTreeM baseContext spec

specToRunTreeVariable :: BaseContext -> Free (SpecCommand BaseContext IO) () -> STM [RunNode BaseContext]
specToRunTreeVariable bc spec = mapM unFixRunTree $ specToRunTree bc spec

isEmptySpec :: forall context. Free (SpecCommand context IO) () -> Bool
isEmptySpec spec = L.null ret
  where context = RunTreeContext {
          runTreeIndexInParent = 0
          , runTreeNumSiblings = 0
          , runTreeCurrentAncestors = mempty
          , runTreeCurrentFolder = Nothing
          }
        (ret, _, _) = runIdentity $ runRWST (specToRunTree' spec) context 0

specToRunTreeM :: (Monad m) => BaseContext -> Free (SpecCommand BaseContext IO) () -> m [RunNodeFixed BaseContext]
specToRunTreeM baseContext spec = do
  let context = RunTreeContext {
        runTreeIndexInParent = 0
        , runTreeNumSiblings = countChildren spec
        , runTreeCurrentAncestors = mempty
        , runTreeCurrentFolder = (</> "results") <$> baseContextRunRoot baseContext
        }
  (ret, _, _) <- runRWST (specToRunTree' spec) context 0
  return ret

-- | Convert a spec to a run tree
specToRunTree' :: (Monad m) => Free (SpecCommand context IO) r -> ConvertM m [RunNodeFixed context]
specToRunTree'  (Free (Before'' loc no l f subspec next)) = do
  common <- getCommon l loc no
  continueWith next =<< RunNodeBefore common <$> recurse l no common subspec <*> pure f
specToRunTree'  (Free (After'' loc no l f subspec next)) = do
  common <- getCommon l loc no
  continueWith next =<< RunNodeAfter common <$> recurse l no common subspec <*> pure f
specToRunTree'  (Free (Introduce'' loc no l _cl alloc cleanup subspec next)) = do
  common <- getCommon l loc no
  continueWith next =<< RunNodeIntroduce common <$> recurse l no common subspec <*> pure alloc <*> pure cleanup
specToRunTree'  (Free (IntroduceWith'' loc no l _cl action subspec next)) = do
  common <- getCommon l loc no
  continueWith next =<< RunNodeIntroduceWith common <$> recurse l no common subspec <*> pure action
specToRunTree'  (Free (Around'' loc no l actionWith subspec next)) = do
  common <- getCommon l loc no
  continueWith next =<< RunNodeAround common <$> recurse l no common subspec <*> pure actionWith
specToRunTree'  (Free (Describe'' loc no l subspec next)) = do
  common <- getCommon l loc no
  continueWith next =<< RunNodeDescribe common <$> recurse l no common subspec
specToRunTree'  (Free (Parallel'' loc no subspec next)) = do
  common <- getCommon "Parallel" loc no
  continueWith next =<< RunNodeParallel common <$> recurse "Parallel" no common subspec
specToRunTree'  (Free (It'' loc no l example next)) = do
  common <- getCommon l loc no
  continueWith next =<< RunNodeIt common <$> pure example
specToRunTree'  (Pure _) = return []


-- * Util

type ConvertM m = RWST RunTreeContext () Int m

getCommon :: (Monad m) => String -> Maybe SrcLoc -> NodeOptions -> ConvertM m RunNodeCommonFixed
getCommon l srcLoc (NodeOptions {..}) = do
  RunTreeContext {..} <- ask

  -- Get a unique ID for this node
  ident <- get
  modify (+1)

  return $ RunNodeCommonWithStatus {
    runTreeLabel = l
    , runTreeId = ident
    , runTreeAncestors = runTreeCurrentAncestors |> ident
    , runTreeToggled = False
    , runTreeOpen = True
    , runTreeStatus = NotStarted
    , runTreeVisible = True
    , runTreeFolder = case (nodeOptionsCreateFolder, runTreeCurrentFolder) of
        (True, Just f) -> Just (f </> (nodeToFolderName l runTreeNumSiblings runTreeIndexInParent))
        _ -> Nothing
    , runTreeVisibilityLevel = nodeOptionsVisibilityThreshold
    , runTreeRecordTime = nodeOptionsRecordTime
    , runTreeLogs = mempty
    , runTreeLoc = srcLoc
    }

continueWith :: (Monad m) => Free (SpecCommand context IO) r -> RunNodeFixed context -> ConvertM m [RunNodeFixed context]
continueWith next node = do
  rest <- local (\rtc -> rtc { runTreeIndexInParent = (runTreeIndexInParent rtc) + 1 }) $ specToRunTree' next
  return (node : rest)

recurse :: (Monad m) => String -> NodeOptions -> RunNodeCommonFixed -> Free (SpecCommand context IO) r -> ConvertM m [RunNodeFixed context]
recurse l (NodeOptions {..}) (RunNodeCommonWithStatus {..}) subspec = local
  (\rtc ->
     if | nodeOptionsCreateFolder ->
          rtc { runTreeCurrentFolder = case runTreeCurrentFolder rtc of
                  Nothing -> Nothing
                  Just f -> Just (f </> (nodeToFolderName l (runTreeNumSiblings rtc) (runTreeIndexInParent rtc)))
              , runTreeIndexInParent = 0
              , runTreeNumSiblings = countImmediateFolderChildren subspec
              , runTreeCurrentAncestors = runTreeAncestors
              }
        | otherwise ->
          rtc { runTreeCurrentAncestors = runTreeAncestors }
  )
  (specToRunTree' subspec)
