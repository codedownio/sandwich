{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The core Spec/SpecCommand types, used to define the test free monad.

module Test.Sandwich.Types.Spec where

import Control.Exception.Safe
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.Free.TH
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Functor.Classes
import Data.Sequence hiding ((:>))
import Data.String.Interpolate
import GHC.Stack
import GHC.TypeLits
import Test.Sandwich.Types.Options
import Test.Sandwich.Types.Util

-- * ExampleM monad

newtype ExampleT context m a = ExampleT { unExampleT :: ReaderT context (ExceptT FailureReason (LoggingT m)) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader context, MonadError FailureReason, MonadLogger)
type ExampleM context = ExampleT context IO

instance (MonadBase b m) => MonadBase b (ExampleT context m) where
  liftBase = liftBaseDefault

instance MonadTrans (ExampleT context) where
  lift x = ExampleT $ ReaderT (\_ -> ExceptT $ liftM Right (LoggingT (\_ -> x)))

instance MonadTransControl (ExampleT context) where
  type StT (ExampleT context) a = StT LoggingT (StT (ExceptT FailureReason) (StT (ReaderT context) a))
  liftWith = defaultLiftWith3 ExampleT unExampleT
  restoreT = defaultRestoreT3 ExampleT

instance (MonadBaseControl b m) => MonadBaseControl b (ExampleT context m) where
  type StM (ExampleT context m) a = ComposeSt (ExampleT context) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

-- * Results

data Result = Success
            | Failure FailureReason
  deriving (Show, Eq)

data FailureReason = Reason (Maybe CallStack) String
                   | ExpectedButGot (Maybe CallStack) String String
                   | DidNotExpectButGot (Maybe CallStack) String
                   | GotException (Maybe String) SomeExceptionWithEq
                   | Pending (Maybe CallStack) (Maybe String)
                   | GetContextException SomeExceptionWithEq
                   | GotAsyncException (Maybe String) SomeAsyncExceptionWithEq
  deriving (Show, Typeable, Eq)

instance Exception FailureReason

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


-- * Base context

data PathSegment = PathSegment {
  pathSegmentName :: String
  , pathSegmentIsContextManager :: Bool
  }

data BaseContext = BaseContext { baseContextPath :: Seq PathSegment
                               , baseContextRunRoot :: Maybe FilePath
                               , baseContextOptions :: Options }

class HasBaseContext a where
  getBaseContext :: a -> BaseContext
  modifyBaseContext :: a -> (BaseContext -> BaseContext) -> a

instance HasBaseContext BaseContext where
  getBaseContext = id
  modifyBaseContext x f = f x

instance HasBaseContext context => HasBaseContext (intro :> context) where
  getBaseContext (_ :> ctx) = getBaseContext ctx
  modifyBaseContext (intro :> ctx) f = intro :> modifyBaseContext ctx f

data Label (l :: Symbol) a = Label

data LabelValue (l :: Symbol) a = LabelValue a

-- TODO: get rid of overlapping instance
-- Maybe look at https://kseo.github.io/posts/2017-02-05-avoid-overlapping-instances-with-closed-type-families.html
class HasLabel context (l :: Symbol) a where
  getLabelValue :: Label l a -> context -> a
instance HasLabel (LabelValue l a) l a where
  getLabelValue _ (LabelValue x) = x
instance {-# OVERLAPPING #-} HasLabel (LabelValue l a :> context) l a where
  getLabelValue _ (LabelValue x :> _) = x
instance {-# OVERLAPPING #-} HasLabel context l a => HasLabel (intro :> context) l a where
  getLabelValue l (_ :> ctx) = getLabelValue l ctx


-- * Free monad language

data (a :: *) :> (b :: *) = a :> b
  deriving Show
infixr :>

type ActionWith a = a -> IO ()

data SpecCommand context next where
  Before :: { label :: String
            , action :: ExampleM context ()
            , subspec :: Spec context ()
            , next :: next } -> SpecCommand context next

  After :: { label :: String
           , action :: ExampleM context ()
           , subspec :: Spec context ()
           , next :: next } -> SpecCommand context next

  Introduce :: { label :: String
               , contextLabel :: Label l intro
               , allocate :: ExampleM context intro
               , cleanup :: ExampleM (LabelValue l intro :> context) ()
               , subspecAugmented :: Spec (LabelValue l intro :> context) ()
               , next :: next } -> SpecCommand context next

  Around :: { label :: String
            , actionWith :: (IO () -> ExampleM context ())
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

type SpecM context a = Free (SpecCommand context) a

type TopSpec = Spec BaseContext ()

makeFree_ ''SpecCommand

instance Show1 (SpecCommand context) where
  liftShowsPrec sp _ d (Before {..}) = showsUnaryWith sp [i|Before[#{label}]<#{show subspec}>|] d next
  liftShowsPrec sp _ d (After {..}) = showsUnaryWith sp [i|After[#{label}]<#{show subspec}>|] d next
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
  -> (ExampleM context ())
  -- ^ Action to perform
  -> SpecWith context
  -- ^ Child spec tree
  -> SpecM context ()

-- | Perform an action before each example in a given spec tree.
beforeEach ::
  String
  -> (ExampleM context ())
  -> Spec context ()
  -> SpecM context ()
beforeEach l f (Free x@(Before {..})) = Free (x { subspec = beforeEach l f subspec, next = beforeEach l f next })
beforeEach l f (Free x@(After {..})) = Free (x { subspec = beforeEach l f subspec, next = beforeEach l f next })
beforeEach l f (Free x@(Around {..})) = Free (x { subspec = beforeEach l f subspec, next = beforeEach l f next })
beforeEach l f (Free x@(Describe {..})) = Free (x { subspec = beforeEach l f subspec, next = beforeEach l f next })
beforeEach l f (Free x@(DescribeParallel {..})) = Free (x { subspec = beforeEach l f subspec, next = beforeEach l f next })
beforeEach l f (Free x@(It {..})) = Free (Before l f (Free (x { next = Pure () })) (beforeEach l f next))
beforeEach _ _ (Pure x) = Pure x
beforeEach l f (Free (Introduce li cl alloc clean subspec next)) = Free (Introduce li cl alloc clean (beforeEach l f' subspec) (beforeEach l f next))
  where f' = do
          let ExampleT r = f
          ExampleT $ withReaderT (\(_ :> context) -> context) r

-- | Perform an action after each example in a given spec tree.
afterEach ::
  String
  -> (ExampleM context ())
  -> Spec context ()
  -> SpecM context ()
afterEach l f (Free x@(Before {..})) = Free (x { subspec = afterEach l f subspec, next = afterEach l f next })
afterEach l f (Free x@(After {..})) = Free (x { subspec = afterEach l f subspec, next = afterEach l f next })
afterEach l f (Free x@(Around {..})) = Free (x { subspec = afterEach l f subspec, next = afterEach l f next })
afterEach l f (Free x@(Describe {..})) = Free (x { subspec = afterEach l f subspec, next = afterEach l f next })
afterEach l f (Free x@(DescribeParallel {..})) = Free (x { subspec = afterEach l f subspec, next = afterEach l f next })
afterEach l f (Free x@(It {..})) = Free (After l f (Free (x { next = Pure () })) (afterEach l f next))
afterEach _ _ (Pure x) = Pure x
afterEach l f (Free (Introduce li cl alloc clean subspec next)) = Free (Introduce li cl alloc clean (afterEach l f' subspec) (afterEach l f next))
  where f' = do
          let ExampleT r = f
          ExampleT $ withReaderT (\(_ :> context) -> context) r

aroundEach ::
  String
  -> (IO () -> ExampleM context ())
  -> Spec context ()
  -> SpecM context ()
aroundEach l f (Free x@(Before {..})) = Free (x { subspec = aroundEach l f subspec, next = aroundEach l f next })
aroundEach l f (Free x@(After {..})) = Free (x { subspec = aroundEach l f subspec, next = aroundEach l f next })
aroundEach l f (Free x@(Around {..})) = Free (x { subspec = aroundEach l f subspec, next = aroundEach l f next })
aroundEach l f (Free x@(Describe {..})) = Free (x { subspec = aroundEach l f subspec, next = aroundEach l f next })
aroundEach l f (Free x@(DescribeParallel {..})) = Free (x { subspec = aroundEach l f subspec, next = aroundEach l f next })
aroundEach l f (Free x@(It {..})) = Free (Around l f (Free (x { next = Pure () })) (aroundEach l f next))
aroundEach _ _ (Pure x) = Pure x
aroundEach l f (Free (Introduce li cl alloc clean subspec next)) = Free (Introduce li cl alloc clean (aroundEach l f' subspec) (aroundEach l f next))
  where
    f' action = do
      let ExampleT r = f action
      ExampleT $ withReaderT (\(_ :> context) -> context) r
