
-- | Miscellaneous exports that need to be exposed, but aren't super interesting.
-- Gathered here to avoid cluttering other files.

module Test.Sandwich.Misc (
  -- * The example monad
  ExampleT
  , ExampleM

  -- * Spec types
  , Spec
  , SpecFree
  , CoreSpec
  , TopSpec
  , TopSpecWithOptions
  , TopSpecWithOptions'
  , isEmptySpec

  -- * Command line options
  , CommandLineOptions(..)
  , CommandLineSlackOptions(..)
  , CommandLineWebdriverOptions(..)
  , BrowserToUse(..)
  , DisplayType(..)
  , commandLineOptionsWithInfo

  -- * Labels
  , Label(..)
  , LabelValue(..)
  , HasLabel
  , (:>)

  -- * Context classes
  , BaseContext
  , HasBaseContext
  , HasCommandLineOptions

  -- * Result types
  , Result(..)
  , FailureReason(..)
  , SomeExceptionWithCallStack(..)
  , SomeExceptionWithEq(..)
  , ExitReason(..)
  ) where

import Test.Sandwich.ArgParsing
import Test.Sandwich.Interpreters.RunTree
import Test.Sandwich.Types.ArgParsing
import Test.Sandwich.Types.General
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.Spec
