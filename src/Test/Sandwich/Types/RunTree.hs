{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
-- |

module Test.Sandwich.Types.RunTree where

import Data.IORef
import Data.Time.Clock
import Test.Sandwich.Types.Example

data Status = NotStarted
            | Running UTCTime
            | Done Result

type RunTreeStatus = IORef Status

instance Show RunTreeStatus where
  show _ = "STATUS"

data RunTreeWithStatus a =
  RunTreeGroup { runTreeLabel :: String
               , runTreeStatus :: a
               , runTreeChildren :: [RunTreeWithStatus a]
               }
  | RunTreeSingle { runTreeLabel :: String
                  , runTreeStatus :: a
                  }
  deriving (Show, Functor)

type RunTree = RunTreeWithStatus (RunTreeStatus)
type RunTreeFixed = RunTreeWithStatus (Status)
