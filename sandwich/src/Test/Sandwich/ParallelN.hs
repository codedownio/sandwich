{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Limiting how much of a test tree runs at once.

module Test.Sandwich.ParallelN (
  -- * Limiting a parallel node
  parallelN
  , parallelN'

  , parallelNFromArgs
  , parallelNFromArgs'

  -- * Lower-level
  , withParallelLanes
  , withParallelLanesFromArgs

  , withParallelLane
  , takeParallelLane

  , defaultParallelNodeOptions

  -- * Types
  , ParallelLanes
  , parallelLanes
  , HasParallelLanes
  ) where

import Control.Concurrent.QSem
import Control.Concurrent.STM (retry)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader
import qualified Data.List as L
import Data.String.Interpolate
import qualified Data.Text as T
import Test.Sandwich.Contexts
import Test.Sandwich.TestTimer
import Test.Sandwich.Types.ArgParsing
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
import UnliftIO.Exception
import UnliftIO.STM


-- * Types

-- | A pool of lanes. Each lane is held by one part of the tree at a time, and has a test timer
-- profile of its own.
data ParallelLanes = ParallelLanes {
  parallelLanesSource :: TimingLaneSource
  -- | The bound. Gating on this first hands lanes out in wait order.
  , parallelLanesSem :: QSem
  , parallelLanesFree :: TVar [Int]
  , parallelLanesProfileNames :: [T.Text]
  }

parallelLanes :: Label "parallelLanes" ParallelLanes
parallelLanes = Label

type HasParallelLanes context = HasLabel context "parallelLanes" ParallelLanes

defaultParallelNodeOptions :: NodeOptions
defaultParallelNodeOptions = defaultNodeOptions { nodeOptionsVisibilityThreshold = 70 }

-- Options for nodes that only introduce or claim lanes. Timing them would put a frame outside
-- every lane, forcing a profile of its own.
laneNodeOptions :: NodeOptions
laneNodeOptions = defaultNodeOptions {
  nodeOptionsRecordTime = False
  , nodeOptionsCreateFolder = False
  , nodeOptionsVisibilityThreshold = 125
  }

-- * Functions

-- | Like 'parallel', but limits the parallelism to N tests at a time.
--
-- Introduces a pool of N lanes, one of which each test claims while it runs. The pool is shared by
-- the whole subtree, so nested 'parallel' nodes are limited too, and each lane is a test timer
-- profile, so you get N profiles rather than one per test.
parallelN :: (
  MonadUnliftIO m, HasBaseContext context
  )
  -- | Number of lanes
  => Int
  -> SpecFree (LabelValue "parallelLanes" ParallelLanes :> context) m ()
  -> SpecFree context m ()
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
  withParallelLanes'' getLanes $
    parallel' nodeOptions $
      aroundEach' Nothing laneNodeOptions "Take parallel lane" (withParallelLane . void) children

-- | Introduce a pool of N lanes, to be claimed with 'withParallelLane' or 'takeParallelLane'.
--
-- Use this when the tests to limit aren't in one place you can wrap with 'parallelN', such as ones
-- from 'Test.Sandwich.TH.getSpecFromFolder'.
withParallelLanes :: (
  MonadIO m, HasBaseContext context
  )
  -- | Number of lanes
  => Int
  -> SpecFree (LabelValue "parallelLanes" ParallelLanes :> context) m ()
  -> SpecFree context m ()
withParallelLanes n = withParallelLanes'' (pure n)

-- | Same as 'withParallelLanes', but extracts the number of lanes from the command line options.
withParallelLanesFromArgs :: forall context a m. (
  MonadIO m, HasBaseContext context, HasCommandLineOptions context a
  )
  -- | Callback to extract the number of lanes
  => (CommandLineOptions a -> Int)
  -> SpecFree (LabelValue "parallelLanes" ParallelLanes :> context) m ()
  -> SpecFree context m ()
withParallelLanesFromArgs getParallelism =
  withParallelLanes'' (getParallelism <$> getContext commandLineOptions)

withParallelLanes'' :: (
  MonadIO m, HasBaseContext context
  )
  => ExampleT context m Int
  -> SpecFree (LabelValue "parallelLanes" ParallelLanes :> context) m ()
  -> SpecFree context m ()
withParallelLanes'' getLanes =
  introduce' laneNodeOptions "Introduce parallel lanes" parallelLanes alloc (const $ return ())
  where
    alloc = do
      numLanes <- max 1 <$> getLanes
      source <- newTimingLaneSource
      sem <- liftIO $ newQSem numLanes
      free <- newTVarIO [0 .. numLanes - 1]
      -- Name the lanes after the profile we're in, so lanes introduced inside another lane (or
      -- inside one branch of a parallel node) don't collide with ones introduced elsewhere.
      baseProfile <- asks getBaseContext >>= currentTestTimerProfile
      let names = [baseProfile <> [i|-lane-#{leftPadWithZeros numLanes lane}|]
                  | lane <- [0 .. numLanes - 1]]
      return $ ParallelLanes source sem free names

    leftPadWithZeros :: Int -> Int -> String
    leftPadWithZeros total num =
      L.replicate (L.length (show (total - 1)) - L.length (show num)) '0' <> show num

-- | Claim a lane, run the action, and release it. Blocks until a lane is free. Everything the
-- action runs, at any depth, is timed under the lane's profile.
--
-- A no-op if this part of the tree already holds a lane from the same pool, since claiming a
-- second one could deadlock.
withParallelLane :: (
  MonadUnliftIO m, HasBaseContextMonad context m, HasParallelLanes context
  ) => m a -> m a
withParallelLane action = do
  pool <- getContext parallelLanes
  inTimingLane (parallelLanesSource pool) >>= \case
    True -> action
    False -> bracket (claimLane pool) (releaseLane pool) $ \lane ->
      withTimingLane (parallelLanesSource pool) (parallelLanesProfileNames pool !! lane) action

-- | 'withParallelLane' as a spec node, designed for use with
-- 'Test.Sandwich.TH.getSpecIndividualSpecHooks'.
takeParallelLane :: (
  MonadUnliftIO m, HasBaseContext context, HasParallelLanes context
  )
  -- | Ignored
  => FilePath
  -> SpecFree context m ()
  -> SpecFree context m ()
takeParallelLane _ = around' laneNodeOptions "Take parallel lane" (withParallelLane . void)

claimLane :: (MonadIO m) => ParallelLanes -> m Int
claimLane (ParallelLanes {parallelLanesSem, parallelLanesFree}) = liftIO $ do
  waitQSem parallelLanesSem
  flip onException (signalQSem parallelLanesSem) $ atomically $ readTVar parallelLanesFree >>= \case
    -- Can't happen: the semaphore already limits us to the number of lanes.
    [] -> retry
    (lane:rest) -> writeTVar parallelLanesFree rest >> return lane

releaseLane :: (MonadIO m) => ParallelLanes -> Int -> m ()
releaseLane (ParallelLanes {parallelLanesSem, parallelLanesFree}) lane = liftIO $ do
  atomically $ modifyTVar' parallelLanesFree (lane :)
  signalQSem parallelLanesSem
