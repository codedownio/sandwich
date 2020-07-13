{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
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
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


specToRunTree :: BaseContext -> Free (SpecCommand BaseContext IO) () -> [RunNodeFixed BaseContext]
specToRunTree baseContext spec = runIdentity $ specToRunTreeM baseContext spec

specToRunTreeVariable :: BaseContext -> Free (SpecCommand BaseContext IO) () -> STM [RunNode BaseContext]
specToRunTreeVariable bc spec = mapM unFixRunTree $ specToRunTree bc spec

specToRunTreeM :: (Monad m) => BaseContext -> Free (SpecCommand BaseContext IO) () -> m [RunNodeFixed BaseContext]
specToRunTreeM baseContext spec = do
  let context = RunTreeContext {
        runTreeOptions = baseContextOptions baseContext
        , runTreeIndexInParent = 0
        , runTreeNumSiblings = countChildren spec
        , runTreeCurrentAncestors = mempty
        , runTreeCurrentFolder = baseContextRunRoot baseContext
        }
  (ret, _, _) <- runRWST (specToRunTree' spec) context 0
  return ret

-- | Convert a spec to a run tree
specToRunTree' :: (Monad m, HasBaseContext context) => Free (SpecCommand context IO) r -> ConvertM m [RunNodeFixed context]
specToRunTree'  (Free (Before l f subspec next)) = do
  common <- getCommon l 100
  continueWith next =<< RunNodeBefore <$> pure common <*> recurse l common subspec <*> pure f
specToRunTree'  (Free (After l f subspec next)) = do
  common <- getCommon l 100
  continueWith next =<< RunNodeAfter <$> pure common <*> recurse l common subspec <*> pure f
specToRunTree'  (Free (Introduce l cl alloc cleanup subspec next)) = do
  common <- getCommon l 100
  continueWith next =<< RunNodeIntroduce <$> pure common <*> recurse l common subspec <*> pure alloc <*> pure cleanup
specToRunTree'  (Free (IntroduceWith l _cl action subspec next)) = do
  common <- getCommon l 100
  continueWith next =<< RunNodeIntroduceWith <$> pure common <*> recurse l common subspec <*> pure action
specToRunTree'  (Free (Around l actionWith subspec next)) = do
  common <- getCommon l 100
  continueWith next =<< RunNodeAround <$> pure common <*> recurse l common subspec <*> pure actionWith
specToRunTree'  (Free (Describe l subspec next)) = do
  common <- getCommon l 100
  continueWith next =<< RunNodeDescribe <$> pure common <*> recurse l common subspec
specToRunTree'  (Free (Parallel subspec next)) = do
  common <- getCommon "Parallel" 100
  continueWith next =<< RunNodeParallel <$> pure common <*> recurse "Parallel" common subspec
specToRunTree'  (Free (It l example next)) =
  continueWith next =<< RunNodeIt <$> getCommon l 100 <*> pure example
specToRunTree'  (Pure _) = return []


-- * Util

type ConvertM m = RWST RunTreeContext () Int m

getCommon :: (Monad m) => String -> Int -> ConvertM m RunNodeCommonFixed
getCommon l visibilityLevel = do
  rtc@(RunTreeContext {..}) <- ask

  -- Get a unique ID for this node
  ident <- get
  modify (+1)

  return $ RunNodeCommonWithStatus {
    runTreeLabel = l
    , runTreeId = ident
    , runTreeAncestors = runTreeCurrentAncestors |> ident
    , runTreeToggled = False
    , runTreeStatus = NotStarted
    , runTreeFolder = appendFolder rtc l
    , runTreeVisibilityLevel = visibilityLevel
    , runTreeLogs = mempty
    }

continueWith :: (Monad m, HasBaseContext context) => Free (SpecCommand context IO) r -> RunNodeFixed context -> ConvertM m [RunNodeFixed context]
continueWith next node = do
  rest <- local (\rtc -> rtc { runTreeIndexInParent = (runTreeIndexInParent rtc) + 1}) $ specToRunTree' next
  return (node : rest)

recurse :: (Monad m, HasBaseContext context) => String -> RunNodeCommonFixed -> Free (SpecCommand context IO) r -> ConvertM m [RunNodeFixed context]
recurse l (RunNodeCommonWithStatus {..}) subspec = local (\rtc -> rtc { runTreeCurrentFolder = appendFolder rtc l
                                                                      , runTreeCurrentAncestors = runTreeAncestors }) (specToRunTree' subspec)
