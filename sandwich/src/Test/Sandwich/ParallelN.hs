{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Wrapper around 'parallel' for limiting the number of children that run at once.

module Test.Sandwich.ParallelN (
  parallelN
  , parallelN'

  , parallelNFromArgs
  , parallelNFromArgs'

  , defaultParallelNodeOptions

  -- * Types
  , parallelismLimit
  , HasParallelismLimit
  , ParallelismLimit(..)
  ) where

import Test.Sandwich.Contexts
import Test.Sandwich.Types.ArgParsing
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec


-- * Types

parallelismLimit :: Label "parallelismLimit" ParallelismLimit
parallelismLimit = Label

type HasParallelismLimit context = HasLabel context "parallelismLimit" ParallelismLimit

defaultParallelNodeOptions :: NodeOptions
defaultParallelNodeOptions = defaultNodeOptions { nodeOptionsVisibilityThreshold = 70 }

-- * Functions

-- | Wrapper around 'parallel' which limits the parallelism to N children at a time.
parallelN :: (
  Monad m
  ) => Int -> SpecFree (LabelValue "parallelismLimit" ParallelismLimit :> context) m () -> SpecFree context m ()
parallelN = parallelN' defaultParallelNodeOptions

parallelN' :: (
  Monad m
  )
  -- | Node options
  => NodeOptions
  -- | Number of children to run at once
  -> Int
  -> SpecFree (LabelValue "parallelismLimit" ParallelismLimit :> context) m ()
  -> SpecFree context m ()
parallelN' nodeOptions n = parallelN'' nodeOptions (pure n)

-- | Same as 'parallelN', but extracts the limit from the command line options.
parallelNFromArgs :: forall context a m. (
  Monad m, HasCommandLineOptions context a
  )
  -- | Callback to extract the limit
  => (CommandLineOptions a -> Int)
  -> SpecFree (LabelValue "parallelismLimit" ParallelismLimit :> context) m ()
  -> SpecFree context m ()
parallelNFromArgs = parallelNFromArgs' @context @a defaultParallelNodeOptions

parallelNFromArgs' :: forall context a m. (
  Monad m, HasCommandLineOptions context a
  )
  -- | Node options
  => NodeOptions
  -- | Callback to extract the limit
  -> (CommandLineOptions a -> Int)
  -> SpecFree (LabelValue "parallelismLimit" ParallelismLimit :> context) m ()
  -> SpecFree context m ()
parallelNFromArgs' nodeOptions getParallelism = parallelN'' nodeOptions (getParallelism <$> getContext commandLineOptions)

parallelN'' :: (
  Monad m
  )
  => NodeOptions
  -> ExampleT context m Int
  -> SpecFree (LabelValue "parallelismLimit" ParallelismLimit :> context) m ()
  -> SpecFree context m ()
parallelN'' nodeOptions getLimit children =
  -- Introducing a 'ParallelismLimit' is picked up by the 'parallel' node, which
  -- uses it to run at most N children at a time (and to share test timer
  -- profiles between them).
  introduce "Introduce parallelism limit" parallelismLimit (ParallelismLimit <$> getLimit) (const $ return ()) $
    parallel' nodeOptions children
