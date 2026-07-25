{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Wrappers around 'parallel' for limiting how much runs at once.

module Test.Sandwich.ParallelN (
  -- * Limiting a parallel node
  parallelN
  , parallelN'

  , parallelNFromArgs
  , parallelNFromArgs'

  -- * Limiting a spec tree you can't wrap directly
  , withParallelLanes
  , withParallelLanesFromArgs

  , takeParallelLane
  , withParallelLane

  , defaultParallelNodeOptions

  -- * Types
  , ParallelLanes
  , parallelLanes
  , HasParallelLanes

  , parallelSemaphore
  , HasParallelSemaphore
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
import Test.Sandwich.Types.TestTimer
import UnliftIO.Exception
import UnliftIO.STM


-- * Types

-- | A pool of lanes, introduced by 'parallelN' or 'withParallelLanes'. A lane is held by one part
-- of the tree at a time, and carries a test timer profile name, so everything running in it shares
-- a profile.
data ParallelLanes = ParallelLanes {
  parallelLanesSource :: TimingLaneSource
  -- | The bound itself. Also handed out under the 'parallelSemaphore' label, so that code claiming
  -- the semaphore directly is limited by the same thing as code using 'withParallelLane'.
  , parallelLanesSem :: QSem
  , parallelLanesFree :: TVar [Int]
  , parallelLanesProfileNames :: [T.Text]
  }

parallelLanes :: Label "parallelLanes" ParallelLanes
parallelLanes = Label

type HasParallelLanes context = HasLabel context "parallelLanes" ParallelLanes

parallelSemaphore :: Label "parallelSemaphore" QSem
parallelSemaphore = Label

type HasParallelSemaphore context = HasLabel context "parallelSemaphore" QSem

defaultParallelNodeOptions :: NodeOptions
defaultParallelNodeOptions = defaultNodeOptions { nodeOptionsVisibilityThreshold = 70 }

-- | Options for the nodes that only exist to introduce or claim lanes. Timing them would put a
-- frame outside every lane, which forces a test timer profile of its own -- exactly the clutter
-- lanes are meant to avoid.
laneNodeOptions :: NodeOptions
laneNodeOptions = defaultNodeOptions {
  nodeOptionsRecordTime = False
  , nodeOptionsCreateFolder = False
  , nodeOptionsVisibilityThreshold = 125
  }

-- * Functions

-- | Wrapper around 'parallel'. Introduces a pool of N lanes and has each test claim one while it
-- runs, so no more than N tests run at once.
--
-- The pool is shared by the whole subtree, no matter how deeply nested, so nested 'parallel' nodes
-- are limited too.
--
-- Each lane is also a test timer profile, so the timing profile stays readable: you get N profiles
-- rather than one per test.
--
-- If your specs come from somewhere this can't wrap directly, such as
-- 'Test.Sandwich.TH.getSpecFromFolder', use 'withParallelLanes' and 'takeParallelLane' instead.
parallelN :: (
  MonadUnliftIO m, HasBaseContext context
  )
  -- | Number of lanes
  => Int
  -> SpecFree (LabelValue "parallelSemaphore" QSem :> LabelValue "parallelLanes" ParallelLanes :> context) m ()
  -> SpecFree context m ()
parallelN = parallelN' defaultParallelNodeOptions

parallelN' :: (
  MonadUnliftIO m, HasBaseContext context
  )
  -- | Node options
  => NodeOptions
  -- | Number of lanes
  -> Int
  -> SpecFree (LabelValue "parallelSemaphore" QSem :> LabelValue "parallelLanes" ParallelLanes :> context) m ()
  -> SpecFree context m ()
parallelN' nodeOptions n = parallelN'' nodeOptions (pure n)

-- | Same as 'parallelN', but extracts the number of lanes from the command line options.
parallelNFromArgs :: forall context a m. (
  MonadUnliftIO m, HasBaseContext context, HasCommandLineOptions context a
  )
  -- | Callback to extract the number of lanes
  => (CommandLineOptions a -> Int)
  -> SpecFree (LabelValue "parallelSemaphore" QSem :> LabelValue "parallelLanes" ParallelLanes :> context) m ()
  -> SpecFree context m ()
parallelNFromArgs = parallelNFromArgs' @context @a defaultParallelNodeOptions

parallelNFromArgs' :: forall context a m. (
  MonadUnliftIO m, HasBaseContext context, HasCommandLineOptions context a
  )
  -- | Node options
  => NodeOptions
  -- | Callback to extract the number of lanes
  -> (CommandLineOptions a -> Int)
  -> SpecFree (LabelValue "parallelSemaphore" QSem :> LabelValue "parallelLanes" ParallelLanes :> context) m ()
  -> SpecFree context m ()
parallelNFromArgs' nodeOptions getParallelism =
  parallelN'' nodeOptions (getParallelism <$> getContext commandLineOptions)

parallelN'' :: (
  MonadUnliftIO m, HasBaseContext context
  )
  => NodeOptions
  -> ExampleT context m Int
  -> SpecFree (LabelValue "parallelSemaphore" QSem :> LabelValue "parallelLanes" ParallelLanes :> context) m ()
  -> SpecFree context m ()
parallelN'' nodeOptions getLanes children =
  withParallelLanes'' getLanes $
    -- Hand out the pool's semaphore under the old label, so specs can still claim the bound
    -- directly. They just don't get the lane's timer profile if they do.
    introduce' laneNodeOptions "Introduce parallel semaphore" parallelSemaphore (parallelLanesSem <$> getContext parallelLanes) (const $ return ()) $
      parallel' nodeOptions $
        aroundEach' Nothing laneNodeOptions "Take parallel lane" (withParallelLane . void) children

-- | Introduce a pool of N lanes for the spec tree below, to be claimed with 'takeParallelLane' or
-- 'withParallelLane'. Nothing is limited until something claims a lane.
--
-- Use this when the tests you want to limit aren't in one place you can wrap with 'parallelN' --
-- for example when they come from 'Test.Sandwich.TH.getSpecFromFolder', where you can pass
-- 'takeParallelLane' as the individual spec hook.
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

-- | Claim a lane for a whole spec tree. Shaped to be passed as
-- 'Test.Sandwich.TH.getSpecIndividualSpecHooks', so it takes (and ignores) the discovered module's
-- path.
--
-- Put this above the node you want inside the lane rather than inside it: anything above the claim
-- can't be in the lane, and gets a test timer profile of its own.
takeParallelLane :: (
  MonadUnliftIO m, HasBaseContext context, HasParallelLanes context
  )
  -- | Ignored
  => FilePath
  -> SpecFree context m ()
  -> SpecFree context m ()
takeParallelLane _ = around' laneNodeOptions "Take parallel lane" (withParallelLane . void)

-- | Claim one of the lanes introduced by 'parallelN' or 'withParallelLanes', run the given action,
-- and release it. Blocks until a lane is free.
--
-- Everything the action runs, at any depth, is timed under the lane's test timer profile.
--
-- If this part of the tree is already holding a lane from the same pool, the action just runs:
-- claiming a second one while holding one could deadlock, so nesting is allowed and ignored.
withParallelLane :: (
  MonadUnliftIO m, HasBaseContextMonad context m, HasParallelLanes context
  ) => m a -> m a
withParallelLane action = do
  pool <- getContext parallelLanes
  inTimingLane (parallelLanesSource pool) >>= \case
    True -> action
    False -> bracket (claimLane pool) (releaseLane pool) $ \lane ->
      withTimingLane (parallelLanesSource pool) (parallelLanesProfileNames pool !! lane) action

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
