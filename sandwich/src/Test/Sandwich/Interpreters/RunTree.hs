{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
-- |

module Test.Sandwich.Interpreters.RunTree (
  specToRunTree
  , specToRunTreeVariable
  ) where

import Control.Concurrent.STM
import Control.Monad.Free
import Control.Monad.Trans.RWS
import Data.Functor.Identity
import Data.Sequence
import System.FilePath
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.RunTree
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


specToRunTree :: BaseContext -> Free (SpecCommand BaseContext IO) () -> [RunNodeFixed BaseContext]
specToRunTree baseContext spec = runIdentity $ specToRunTreeM baseContext spec

specToRunTreeVariable :: BaseContext -> Free (SpecCommand BaseContext IO) () -> STM [RunNode BaseContext]
specToRunTreeVariable bc spec = mapM unFixRunTree $ specToRunTree bc spec

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
specToRunTree' :: (Monad m, HasBaseContext context) => Free (SpecCommand context IO) r -> ConvertM m [RunNodeFixed context]
specToRunTree'  (Free (Before' no l f subspec next)) = do
  common <- getCommon l no
  continueWith next =<< RunNodeBefore <$> pure common <*> recurse l no common subspec <*> pure f
specToRunTree'  (Free (After' no l f subspec next)) = do
  common <- getCommon l no
  continueWith next =<< RunNodeAfter <$> pure common <*> recurse l no common subspec <*> pure f
specToRunTree'  (Free (Introduce' no l cl alloc cleanup subspec next)) = do
  common <- getCommon l no
  continueWith next =<< RunNodeIntroduce <$> pure common <*> recurse l no common subspec <*> pure alloc <*> pure cleanup
specToRunTree'  (Free (IntroduceWith' no l _cl action subspec next)) = do
  common <- getCommon l no
  continueWith next =<< RunNodeIntroduceWith <$> pure common <*> recurse l no common subspec <*> pure action
specToRunTree'  (Free (Around' no l actionWith subspec next)) = do
  common <- getCommon l no
  continueWith next =<< RunNodeAround <$> pure common <*> recurse l no common subspec <*> pure actionWith
specToRunTree'  (Free (Describe' no l subspec next)) = do
  common <- getCommon l no
  continueWith next =<< RunNodeDescribe <$> pure common <*> recurse l no common subspec
specToRunTree'  (Free (Parallel' no subspec next)) = do
  common <- getCommon "Parallel" no
  continueWith next =<< RunNodeParallel <$> pure common <*> recurse "Parallel" no common subspec
specToRunTree'  (Free (It' no l example next)) = do
  common <- getCommon l no
  continueWith next =<< RunNodeIt <$> pure common <*> pure example
specToRunTree'  (Pure _) = return []


-- * Util

type ConvertM m = RWST RunTreeContext () Int m

getCommon :: (Monad m) => String -> NodeOptions -> ConvertM m RunNodeCommonFixed
getCommon l (NodeOptions {..}) = do
  rtc@(RunTreeContext {..}) <- ask

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
    , runTreeLogs = mempty
    }

continueWith :: (Monad m, HasBaseContext context) => Free (SpecCommand context IO) r -> RunNodeFixed context -> ConvertM m [RunNodeFixed context]
continueWith next node = do
  rest <- local (\rtc -> rtc { runTreeIndexInParent = (runTreeIndexInParent rtc) + 1 }) $ specToRunTree' next
  return (node : rest)

recurse :: (Monad m, HasBaseContext context) => String -> NodeOptions -> RunNodeCommonFixed -> Free (SpecCommand context IO) r -> ConvertM m [RunNodeFixed context]
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
