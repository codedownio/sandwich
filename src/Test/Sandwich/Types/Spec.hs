{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

-- | The core Spec/SpecCommand types, used to define the test free monad.

module Test.Sandwich.Types.Spec where

import Control.Monad.Free
import Control.Monad.Free.TH
import GHC.Stack
import Test.Sandwich.Types.Example

data (a :: *) :> (b :: *) = a :> b
  deriving Show

data SpecCommand context next where
  Before :: String
         -> (context -> IO ())
         -> Spec context ()
         -> next -> SpecCommand context next

  Introduce :: (Show intro) =>
            String
            -> (context -> IO (intro :> context))
            -> ((intro :> context) -> IO ())
            -> Spec (intro :> context) ()
            -> next -> SpecCommand context next

  Describe :: String
           -> Spec context ()
           -> next -> SpecCommand context next

  DescribeParallel :: String
                   -> Spec context ()
                   -> next -> SpecCommand context next

  It :: (HasCallStack)
     => String
     -> (context -> IO Result)
     -> next -> SpecCommand context next

deriving instance Functor (SpecCommand n)

type Spec context = Free (SpecCommand context)

type TopSpec = Spec () ()

makeFree_ ''SpecCommand
