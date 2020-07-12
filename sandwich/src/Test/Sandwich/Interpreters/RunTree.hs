{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}
-- |

module Test.Sandwich.Interpreters.RunTree (
  specToRunTree
  ) where

import Control.Monad.Free
import Control.Monad.Trans.Reader
import Data.Functor.Identity
import Data.Sequence
import Test.Sandwich.Interpreters.RunTree.Util
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


specToRunTree :: BaseContext -> Free (SpecCommand BaseContext IO) () -> [RunNode BaseContext]
specToRunTree baseContext spec = runIdentity $ specToRunTreeM baseContext spec

specToRunTreeM :: (Monad m) => BaseContext -> Free (SpecCommand BaseContext IO) () -> m [RunNode BaseContext]
specToRunTreeM baseContext spec = do
  runReaderT (specToRunTree' spec) $ RunTreeContext {
    runTreeOptions = baseContextOptions baseContext
    , runTreeIndexInParent = 0
    , runTreeNumSiblings = countChildren spec
    , runTreeCurrentFolder = baseContextRunRoot baseContext
    }

-- | Convert a spec to a run tree
specToRunTree' :: (Monad m, HasBaseContext context) => Free (SpecCommand context IO) r -> ReaderT RunTreeContext m [RunNode context]
specToRunTree' (Free (Before l f subspec next)) =
  continueWith next =<< RunNodeBefore <$> getCommon l 100 <*> recurse l subspec <*> pure f
specToRunTree' (Free (After l f subspec next)) =
  continueWith next =<< RunNodeAfter <$> getCommon l 100 <*> recurse l subspec <*> pure f
specToRunTree' (Free (Introduce l _cl alloc cleanup subspec next)) =
  continueWith next =<< RunNodeIntroduce <$> getCommon l 100 <*> recurse l subspec <*> pure alloc <*> pure cleanup
specToRunTree' (Free (IntroduceWith l _cl action subspec next)) =
  continueWith next =<< RunNodeIntroduceWith <$> getCommon l 100 <*> recurse l subspec <*> pure action
specToRunTree' (Free (Around l actionWith subspec next)) =
  continueWith next =<< RunNodeAround <$> getCommon l 100 <*> recurse l subspec <*> pure actionWith
specToRunTree' (Free (Describe l subspec next)) =
  continueWith next =<< RunNodeDescribe <$> getCommon l 100 <*> recurse l subspec
specToRunTree' (Free (Parallel subspec next)) =
  continueWith next =<< RunNodeParallel <$> getCommon "Parallel" 100 <*> recurse "Parallel" subspec
specToRunTree' (Free (It l example next)) =
  continueWith next =<< RunNodeIt <$> getCommon l 100 <*> pure example
specToRunTree' (Pure _) = return []


-- * Util

getCommon :: (Monad m) => String -> Int -> ReaderT RunTreeContext m (RunNodeCommon Status (Seq LogEntry) Bool)
getCommon l visibilityLevel = do
  rtc <- ask

  return $ RunNodeCommon {
    runTreeLabel = l
    , runTreeToggled = False
    , runTreeStatus = NotStarted
    , runTreeFolder = appendFolder rtc l
    , runTreeVisibilityLevel = visibilityLevel
    , runTreeLogs = mempty
    }

continueWith :: (Monad m, HasBaseContext context) => Free (SpecCommand context IO) r -> RunNode context -> ReaderT RunTreeContext m [RunNode context]
continueWith next node = do
  rest <- local (\rtc -> rtc { runTreeIndexInParent = (runTreeIndexInParent rtc) + 1}) $ specToRunTree' next
  return (node : rest)

recurse :: (Monad m, HasBaseContext context) => String -> Free (SpecCommand context IO) r -> ReaderT RunTreeContext m [RunNode context]
recurse l subspec = local (\rtc -> rtc { runTreeCurrentFolder = appendFolder rtc l }) (specToRunTree' subspec)
