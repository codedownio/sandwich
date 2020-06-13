{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}

-- | The core Spec/SpecCommand types, used to define the test free monad.

module Test.Sandwich.Types.Spec where

import Control.Monad.Free
import Control.Monad.Free.TH
import Data.Functor.Classes
import Data.String.Interpolate
import GHC.Stack
import Test.Sandwich.Types.Example

data (a :: *) :> (b :: *) = a :> b
  deriving Show

type ActionWith a = a -> IO ()

data SpecCommand context next where
  Before :: String
         -> (context -> IO ())
         -> Spec context ()
         -> next -> SpecCommand context next

  Introduce :: (Show intro) =>
            String
            -> (context -> IO intro)
            -> ((intro :> context) -> IO ())
            -> Spec (intro :> context) ()
            -> next -> SpecCommand context next

  Around :: (Show intro) =>
            String
            -> (ActionWith (intro :> context) -> IO ())
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
deriving instance Foldable (SpecCommand n)
deriving instance Traversable (SpecCommand n)

type Spec context = Free (SpecCommand context)

type TopSpec = Spec () ()

makeFree_ ''SpecCommand

instance (Show context) => Show1 (SpecCommand context) where
  liftShowsPrec sp _ d (Before s f subspec x) = showsUnaryWith sp [i|Before[#{s}]<#{show subspec}>|] d x
  liftShowsPrec sp _ d (Introduce s alloc clean subspec x) = showsUnaryWith sp [i|Introduce[#{s}]<#{show subspec}>|] d x
  liftShowsPrec sp _ d (Around s f subspec x) = showsUnaryWith sp [i|Around[#{s}]<#{show subspec}>|] d x
  liftShowsPrec sp _ d (Describe s subspec x) = showsUnaryWith sp [i|Describe[#{s}]<#{show subspec}>|] d x
  liftShowsPrec sp _ d (DescribeParallel s subspec x) = showsUnaryWith sp [i|Describe[#{s}]<#{show subspec}>|] d x
  liftShowsPrec sp _ d (It s ex x) = showsUnaryWith sp [i|It[#{s}]|] d x
