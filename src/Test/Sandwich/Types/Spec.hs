{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}

-- | The core Spec/SpecCommand types, used to define the test free monad.

module Test.Sandwich.Types.Spec where

import Control.Exception.Safe
import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.Free.TH
import Control.Monad.Reader
import Data.Functor.Classes
import Data.String.Interpolate
import GHC.Stack

-- * ExampleM monad

newtype ExampleM context a = ExampleM (ReaderT context (ExceptT FailureReason IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader context, MonadError FailureReason)

-- * Results

data Result = Success
            | Pending (Maybe CallStack) (Maybe String)
            | Failure FailureReason
  deriving (Show, Eq)

data FailureReason = Reason (Maybe CallStack) String
                   | ExpectedButGot (Maybe CallStack) String String
                   | DidNotExpectButGot (Maybe CallStack) String
                   | GotException (Maybe String) SomeExceptionWithEq
                   | GetContextException SomeExceptionWithEq
                   | GotAsyncException (Maybe String) SomeAsyncExceptionWithEq
  deriving (Show, Typeable, Eq)

instance Eq CallStack where
  c1 == c2 = show c1 == show c2

newtype SomeExceptionWithEq = SomeExceptionWithEq SomeException
  deriving Show
instance Eq SomeExceptionWithEq where
  (SomeExceptionWithEq e1) == (SomeExceptionWithEq e2) = show e1 == show e2

newtype SomeAsyncExceptionWithEq = SomeAsyncExceptionWithEq SomeAsyncException
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


-- * Free monad language

data (a :: *) :> (b :: *) = a :> b
  deriving Show

type ActionWith a = a -> IO ()

data SpecCommand context next where
  Before :: { label :: String
            , action :: context -> IO ()
            , subspec :: Spec context ()
            , next :: next } -> SpecCommand context next

  After :: { label :: String
           , action :: context -> IO ()
           , subspec :: Spec context ()
           , next :: next } -> SpecCommand context next

  Introduce :: { label :: String
               , allocate :: (context -> IO intro)
               , cleanup :: ((intro :> context) -> IO ())
               , subspecAugmented :: Spec (intro :> context) ()
               , next :: next } -> SpecCommand context next

  Around :: { label :: String
            , actionWith :: (context -> IO () -> IO ())
            , subspec :: Spec context ()
            , next :: next } -> SpecCommand context next

  Describe :: { label :: String
              , subspec :: Spec context ()
              , next :: next } -> SpecCommand context next

  DescribeParallel :: { label :: String
                      , subspec :: Spec context ()
                      , next :: next } -> SpecCommand context next

  It :: (HasCallStack) => { label :: String
                          , example :: ExampleM context ()
                          , next :: next } -> SpecCommand context next

deriving instance Functor (SpecCommand n)
deriving instance Foldable (SpecCommand n)
deriving instance Traversable (SpecCommand n)

type Spec context = Free (SpecCommand context)

type SpecWith context = Spec context ()

-- data SpecM context a = Free (SpecWith context) a
type SpecM context a = Free (SpecCommand context) a

type TopSpec = Spec () ()

makeFree_ ''SpecCommand

instance Show1 (SpecCommand context) where
  liftShowsPrec sp _ d (Before {..}) = showsUnaryWith sp [i|Before[#{label}]<#{show subspec}>|] d next
  liftShowsPrec sp _ d (Introduce {..}) = showsUnaryWith sp [i|Introduce[#{label}]<#{show subspecAugmented}>|] d next
  liftShowsPrec sp _ d (Around {..}) = showsUnaryWith sp [i|Around[#{label}]<#{show subspec}>|] d next
  liftShowsPrec sp _ d (Describe {..}) = showsUnaryWith sp [i|Describe[#{label}]<#{show subspec}>|] d next
  liftShowsPrec sp _ d (DescribeParallel {..}) = showsUnaryWith sp [i|Describe[#{label}]<#{show subspec}>|] d next
  liftShowsPrec sp _ d (It {..}) = showsUnaryWith sp [i|It[#{label}]|] d next

-- First write beforeEach/afterEach to demonstrate push down approach
-- Then think about how/whether we can to introduceEach / aroundEach


-- | Perform an action before a given spec tree.
before :: 
  String
  -- ^ Label for this context manager
  -> (context -> IO ())
  -- ^ Action to perform
  -> SpecWith context
  -- ^ Child spec tree
  -> SpecM context ()

-- | Perform an action before each example in a given spec tree.
beforeEach ::
  String
  -> (context -> IO ())
  -> Spec context ()
  -> SpecM context ()
beforeEach l f (Free x@(Before {..})) = Free (x { subspec = beforeEach l f subspec, next = beforeEach l f next })
beforeEach l f (Free x@(After {..})) = Free (x { subspec = beforeEach l f subspec, next = beforeEach l f next })
beforeEach l f (Free x@(Around {..})) = Free (x { subspec = beforeEach l f subspec, next = beforeEach l f next })
beforeEach l f (Free x@(Describe {..})) = Free (x { subspec = beforeEach l f subspec, next = beforeEach l f next })
beforeEach l f (Free x@(DescribeParallel {..})) = Free (x { subspec = beforeEach l f subspec, next = beforeEach l f next })
beforeEach l f (Free x@(It {..})) = Free (Before l f (Free (x { next = Pure () })) (beforeEach l f next))
beforeEach _ _ (Pure x) = Pure x
beforeEach l f (Free (Introduce li alloc clean subspec next)) = Free (Introduce li alloc clean (beforeEach l f' subspec) (beforeEach l f next))
  where f' (_ :> context) = f context

-- | Perform an action after each example in a given spec tree.
afterEach ::
  String
  -> (context -> IO ())
  -> Spec context ()
  -> SpecM context ()
afterEach l f (Free x@(Before {..})) = Free (x { subspec = afterEach l f subspec, next = afterEach l f next })
afterEach l f (Free x@(After {..})) = Free (x { subspec = afterEach l f subspec, next = afterEach l f next })
afterEach l f (Free x@(Around {..})) = Free (x { subspec = afterEach l f subspec, next = afterEach l f next })
afterEach l f (Free x@(Describe {..})) = Free (x { subspec = afterEach l f subspec, next = afterEach l f next })
afterEach l f (Free x@(DescribeParallel {..})) = Free (x { subspec = afterEach l f subspec, next = afterEach l f next })
afterEach l f (Free x@(It {..})) = Free (After l f (Free (x { next = Pure () })) (afterEach l f next))
afterEach _ _ (Pure x) = Pure x
afterEach l f (Free (Introduce li alloc clean subspec next)) = Free (Introduce li alloc clean (afterEach l f' subspec) (afterEach l f next))
  where f' (_ :> context) = f context

aroundEach ::
  String
  -> (context -> IO () -> IO ())
  -> Spec context ()
  -> SpecM context ()
aroundEach l f (Free x@(Before {..})) = Free (x { subspec = aroundEach l f subspec, next = aroundEach l f next })
aroundEach l f (Free x@(After {..})) = Free (x { subspec = aroundEach l f subspec, next = aroundEach l f next })
aroundEach l f (Free x@(Around {..})) = Free (x { subspec = aroundEach l f subspec, next = aroundEach l f next })
aroundEach l f (Free x@(Describe {..})) = Free (x { subspec = aroundEach l f subspec, next = aroundEach l f next })
aroundEach l f (Free x@(DescribeParallel {..})) = Free (x { subspec = aroundEach l f subspec, next = aroundEach l f next })
aroundEach l f (Free x@(It {..})) = Free (Around l f (Free (x { next = Pure () })) (aroundEach l f next))
aroundEach _ _ (Pure x) = Pure x
aroundEach l f (Free (Introduce li alloc clean subspec next)) = Free (Introduce li alloc clean (aroundEach l f' subspec) (aroundEach l f next))
  where
    f' (_ :> context) = f context


