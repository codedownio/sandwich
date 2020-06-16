{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |

module Test.Sandwich.Types.Example where

import Control.Exception
import Control.Monad.Trans.Reader
import Data.Typeable (Typeable)
import qualified Test.QuickCheck as QC

-- * Example

class Example context e where
  evaluateExample :: e -> ReaderT (ItemContext context) IO Result

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
  | GotAsyncException (Maybe String) AsyncException
  deriving (Show, Typeable, Eq)

data SomeExceptionWithEq = SomeExceptionWithEq SomeException
  deriving Show

instance Eq SomeExceptionWithEq where
  (SomeExceptionWithEq e1) == (SomeExceptionWithEq e2) = show e1 == show e2

-- | @Location@ is used to represent source locations.
data Location = Location {
  locationFile :: FilePath
, locationLine :: Int
, locationColumn :: Int
} deriving (Eq, Show, Read)

isFailure :: Result -> Bool
isFailure (Failure {}) = True
isFailure _ = False
