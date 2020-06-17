{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |

module Test.Sandwich.Types.Example where

import Control.Exception.Safe
import Control.Monad.Trans.Reader
import Data.Typeable (Typeable)
import qualified Test.QuickCheck as QC

-- * Example

-- * Example type

class Example context e where
  runExample :: e -> context -> IO Result

instance Example context (context -> IO Result) where
  runExample action = action

instance Example context (context -> IO ()) where
  runExample action context = do
    action context
    return Success

instance Example context (IO Result) where
  runExample action _ = action

instance Example context (IO ()) where
  runExample action _ = action >> return Success

instance Example context (ExampleM context Result) where
  runExample action _ = undefined

newtype ExampleM context a = ExampleM (ReaderT context IO a)
  deriving (Functor, Applicative, Monad)


-- * Item context

data ItemContext a = ItemContext {
  itemContextParams :: Params
  , itemContextProgressCallback :: Progress -> IO ()
  , itemContextContext :: a
  }

data Params = Params {
  paramsQuickCheckArgs  :: QC.Args
, paramsSmallCheckDepth :: Int
} deriving (Show)

type Progress = (Int, Int)

-- * Results

-- | The result of running an example
data Result =
  Success
  | Pending (Maybe Location) (Maybe String)
  | Failure (Maybe Location) FailureReason
  deriving (Show, Eq)

data FailureReason =
    NoReason
  | Reason String
  | ExpectedButGot String String
  | GotException (Maybe String) SomeExceptionWithEq
  | GetContextException SomeExceptionWithEq
  | GotAsyncException (Maybe String) SomeAsyncExceptionWithEq
  deriving (Show, Typeable, Eq)

data SomeExceptionWithEq = SomeExceptionWithEq SomeException
  deriving Show
instance Eq SomeExceptionWithEq where
  (SomeExceptionWithEq e1) == (SomeExceptionWithEq e2) = show e1 == show e2

data SomeAsyncExceptionWithEq = SomeAsyncExceptionWithEq SomeAsyncException
  deriving Show
instance Eq SomeAsyncExceptionWithEq where
  (SomeAsyncExceptionWithEq e1) == (SomeAsyncExceptionWithEq e2) = show e1 == show e2

-- | @Location@ is used to represent source locations.
data Location = Location {
  locationFile :: FilePath
, locationLine :: Int
, locationColumn :: Int
} deriving (Eq, Show, Read)

isFailure :: Result -> Bool
isFailure (Failure {}) = True
isFailure _ = False
