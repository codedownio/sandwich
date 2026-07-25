{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Wrappers around 'parallel' for limiting the threads using a semaphore.

module Test.Sandwich.ParallelN (
  -- * Limiting with lanes shared by the whole subtree
  parallelN
  , parallelN'

  , parallelNFromArgs
  , parallelNFromArgs'

  , withParallelLane

  -- * Limiting a single parallel node
  , parallelNWithLanes
  , parallelNWithLanes'

  , parallelNWithLanesFromArgs
  , parallelNWithLanesFromArgs'

  , defaultParallelNodeOptions

  -- * Types
  , parallelLanes
  , HasParallelLanes
  , ParallelLanes(..)

  , parallelismLimit
  , HasParallelismLimit
  , ParallelismLimit(..)
  ) where

import Control.Concurrent.STM (retry)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import qualified Data.Text as T
import Test.Sandwich.Contexts
import Test.Sandwich.Types.ArgParsing
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import UnliftIO.Exception
import UnliftIO.STM


-- * Types

parallelLanes :: Label "parallelLanes" ParallelLanes
parallelLanes = Label

type HasParallelLanes context = HasLabel context "parallelLanes" ParallelLanes

parallelismLimit :: Label "parallelismLimit" ParallelismLimit
parallelismLimit = Label

type HasParallelismLimit context = HasLabel context "parallelismLimit" ParallelismLimit

defaultParallelNodeOptions :: NodeOptions
defaultParallelNodeOptions = defaultNodeOptions { nodeOptionsVisibilityThreshold = 70 }

-- * Functions

-- | Wrapper around 'parallel'. Introduces a pool of N lanes and has each test claim one while it
-- runs, so no more than N tests run at once.
--
-- The pool is shared by the whole subtree, no matter how deeply nested, so nested 'parallel' nodes
-- are limited too. Tests in the subtree can claim a lane themselves with 'withParallelLane', which
-- is useful when your specs come from somewhere this function can't wrap directly (such as
-- 'Test.Sandwich.TH.getSpecFromFolder'): claim it wherever the expensive work starts, and
-- everything below that point runs inside the lane.
--
-- Each lane is also a test timer profile, so the profile stays readable: you get N profiles rather
-- than one per test.
parallelN :: (
  MonadUnliftIO m, HasBaseContext context
  ) => Int -> SpecFree (LabelValue "parallelLanes" ParallelLanes :> context) m () -> SpecFree context m ()
parallelN = parallelN' defaultParallelNodeOptions

parallelN' :: (
  MonadUnliftIO m, HasBaseContext context
  )
  -- | Node options
  => NodeOptions
  -- | Number of lanes
  -> Int
  -> SpecFree (LabelValue "parallelLanes" ParallelLanes :> context) m ()
  -> SpecFree context m ()
parallelN' nodeOptions n = parallelN'' nodeOptions (pure n)

-- | Same as 'parallelN', but extracts the number of lanes from the command line options.
parallelNFromArgs :: forall context a m. (
  MonadUnliftIO m, HasBaseContext context, HasCommandLineOptions context a
  )
  -- | Callback to extract the number of lanes
  => (CommandLineOptions a -> Int)
  -> SpecFree (LabelValue "parallelLanes" ParallelLanes :> context) m ()
  -> SpecFree context m ()
parallelNFromArgs = parallelNFromArgs' @context @a defaultParallelNodeOptions

parallelNFromArgs' :: forall context a m. (
  MonadUnliftIO m, HasBaseContext context, HasCommandLineOptions context a
  )
  -- | Node options
  => NodeOptions
  -- | Callback to extract the number of lanes
  -> (CommandLineOptions a -> Int)
  -> SpecFree (LabelValue "parallelLanes" ParallelLanes :> context) m ()
  -> SpecFree context m ()
parallelNFromArgs' nodeOptions getParallelism =
  parallelN'' nodeOptions (getParallelism <$> getContext commandLineOptions)

parallelN'' :: (
  MonadUnliftIO m, HasBaseContext context
  )
  => NodeOptions
  -> ExampleT context m Int
  -> SpecFree (LabelValue "parallelLanes" ParallelLanes :> context) m ()
  -> SpecFree context m ()
parallelN'' nodeOptions getLanes children =
  introduce "Introduce parallel lanes" parallelLanes (ParallelLanes <$> getLanes) (const $ return ()) $
    parallel' nodeOptions $
      aroundEach' Nothing laneNodeOptions "Take parallel lane" (withParallelLane . void) children
  where
    -- Don't time this node: it starts before we have a lane, so its frame would have to go in a
    -- profile of its own, which is exactly the clutter the lanes are meant to avoid.
    laneNodeOptions = defaultNodeOptions { nodeOptionsRecordTime = False }

-- | Claim one of the lanes introduced by 'parallelN', run the given action, and release it. Blocks
-- until a lane is free.
--
-- Everything the action runs, at any depth, is timed under the lane's test timer profile.
--
-- This is a no-op if there are no lanes in scope, or if this part of the tree is already holding
-- one: claiming a second lane while holding one could deadlock, so nesting is allowed and ignored.
withParallelLane :: (MonadUnliftIO m, HasBaseContextMonad context m) => m a -> m a
withParallelLane action = do
  BaseContext {..} <- asks getBaseContext
  case (baseContextLanePool, baseContextCurrentLane) of
    (Nothing, _) -> action
    -- We're not underneath a parallel node, so there's no profile to switch; just take a lane.
    (Just pool, Nothing) -> bracket (claimLane pool) (releaseLane pool) (const action)
    (Just pool, Just currentLane) -> readTVarIO currentLane >>= \case
      Just (LaneState {laneStateHeldPools}) | lanePoolFree pool `elem` laneStateHeldPools -> action
      heldBefore -> bracket (claimLane pool) (releaseLane pool) $ \lane -> do
        let held = LaneState (lanePoolFree pool : maybe [] laneStateHeldPools heldBefore)
                             (laneProfileName pool lane)
        bracket_ (atomically $ writeTVar currentLane (Just held))
                 (atomically $ writeTVar currentLane heldBefore)
                 action

claimLane :: (MonadIO m) => LanePool -> m Int
claimLane (LanePool {lanePoolFree}) = atomically $ readTVar lanePoolFree >>= \case
  [] -> retry
  (lane:rest) -> writeTVar lanePoolFree rest >> return lane

releaseLane :: (MonadIO m) => LanePool -> Int -> m ()
releaseLane (LanePool {lanePoolFree}) lane = atomically $ modifyTVar' lanePoolFree (lane :)

laneProfileName :: LanePool -> Int -> T.Text
laneProfileName (LanePool {lanePoolProfileNames}) lane = lanePoolProfileNames !! lane

-- | Wrapper around 'parallel' which runs at most N of its own children at a time. Each child runs
-- in one of N lanes, claimed just before it starts and released once it's finished, and the
-- children sharing a lane share a test timer profile. So you get N profiles rather than one per
-- child, which is much easier to read in the speedscope viewer.
--
-- Note that this bounds the children of this node only, which is a different thing from what
-- 'parallelN' bounds:
--
--   * Setup nodes inside a child count against the limit, since a lane is held for the child's
--     whole lifetime.
--
--   * 'parallel' nodes nested below a child are /not/ bounded. If your tree is assembled from
--     nested parallel nodes (for example by 'Test.Sandwich.TH.getSpecFromFolder' with a parallel
--     combiner), use 'parallelN' instead, or the limit will only apply to the top level.
parallelNWithLanes :: (
  Monad m
  ) => Int -> SpecFree (LabelValue "parallelismLimit" ParallelismLimit :> context) m () -> SpecFree context m ()
parallelNWithLanes = parallelNWithLanes' defaultParallelNodeOptions

parallelNWithLanes' :: (
  Monad m
  )
  -- | Node options
  => NodeOptions
  -- | Number of lanes
  -> Int
  -> SpecFree (LabelValue "parallelismLimit" ParallelismLimit :> context) m ()
  -> SpecFree context m ()
parallelNWithLanes' nodeOptions n = parallelNWithLanes'' nodeOptions (pure n)

-- | Same as 'parallelNWithLanes', but extracts the number of lanes from the command line options.
parallelNWithLanesFromArgs :: forall context a m. (
  Monad m, HasCommandLineOptions context a
  )
  -- | Callback to extract the number of lanes
  => (CommandLineOptions a -> Int)
  -> SpecFree (LabelValue "parallelismLimit" ParallelismLimit :> context) m ()
  -> SpecFree context m ()
parallelNWithLanesFromArgs = parallelNWithLanesFromArgs' @context @a defaultParallelNodeOptions

parallelNWithLanesFromArgs' :: forall context a m. (
  Monad m, HasCommandLineOptions context a
  )
  -- | Node options
  => NodeOptions
  -- | Callback to extract the number of lanes
  -> (CommandLineOptions a -> Int)
  -> SpecFree (LabelValue "parallelismLimit" ParallelismLimit :> context) m ()
  -> SpecFree context m ()
parallelNWithLanesFromArgs' nodeOptions getParallelism =
  parallelNWithLanes'' nodeOptions (getParallelism <$> getContext commandLineOptions)

parallelNWithLanes'' :: (
  Monad m
  )
  => NodeOptions
  -> ExampleT context m Int
  -> SpecFree (LabelValue "parallelismLimit" ParallelismLimit :> context) m ()
  -> SpecFree context m ()
parallelNWithLanes'' nodeOptions getLimit children =
  -- Introducing a 'ParallelismLimit' is picked up by the 'parallel' node just below, which uses it
  -- to run at most N children at a time (and to share test timer profiles between them). The node
  -- clears it for its own children, so a nested 'parallel' doesn't claim it a second time.
  introduce "Introduce parallelism limit" parallelismLimit (ParallelismLimit <$> getLimit) (const $ return ()) $
    parallel' nodeOptions children
