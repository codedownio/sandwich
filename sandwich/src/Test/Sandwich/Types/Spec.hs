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
import Data.String.Interpolate
import GHC.Stack
import GHC.TypeLits

-- * ExampleM monad

newtype ExampleT context m a = ExampleT { unExampleT :: ReaderT context (LoggingT m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader context, MonadLogger, MonadThrow, MonadCatch, MonadMask)
type ExampleM context = ExampleT context IO

instance (MonadBase b m) => MonadBase b (ExampleT context m) where
  liftBase = liftBaseDefault

instance MonadTrans (ExampleT context) where
  lift x = ExampleT $ ReaderT (\_ -> (LoggingT (\_ -> x)))

instance MonadTransControl (ExampleT context) where
  type StT (ExampleT context) a = StT LoggingT (StT (ReaderT context) a)
  liftWith = defaultLiftWith2 ExampleT unExampleT
  restoreT = defaultRestoreT2 ExampleT

instance (MonadBaseControl b m) => MonadBaseControl b (ExampleT context m) where
  type StM (ExampleT context m) a = ComposeSt (ExampleT context) m a
  liftBaseWith = defaultLiftBaseWith
  restoreM = defaultRestoreM

-- * Results

data Result = Success
            | Failure FailureReason
  deriving (Show, Eq)

data ShowEqBox = forall s. (Show s, Eq s) => SEB s
instance Show ShowEqBox where show (SEB x) = show x
instance Eq ShowEqBox where (SEB x1) == (SEB x2) = show x1 == show x2

data FailureReason = Reason { failureCallStack :: Maybe CallStack
                            , failureReason :: String }
                   | ExpectedButGot { failureCallStack :: Maybe CallStack
                                    , failureValue1 :: ShowEqBox
                                    , failureValue2 :: ShowEqBox }
                   | DidNotExpectButGot { failureCallStack :: Maybe CallStack
                                        , failureValue1 :: ShowEqBox }
                   | GotException { failureCallStack :: Maybe CallStack
                                  , failureMessage :: Maybe String
                                  , failureException :: SomeExceptionWithEq }
                   | Pending { failureCallStack :: Maybe CallStack
                             , failurePendingMessage :: Maybe String }
                   | GetContextException { failureCallStack :: Maybe CallStack
                                         , failureException :: SomeExceptionWithEq }
                   | GotAsyncException { failureCallStack :: Maybe CallStack
                                       , failureMessage :: Maybe String
                                       , failureAsyncException :: SomeAsyncExceptionWithEq }
  deriving (Show, Typeable, Eq)

instance Exception FailureReason

instance Eq CallStack where
  c1 == c2 = show c1 == show c2

newtype SomeExceptionWithEq = SomeExceptionWithEq SomeException
instance Show SomeExceptionWithEq where
  show (SomeExceptionWithEq e) = show e
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

-- * Label stuff

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

data NodeOptions = NodeOptions {
  nodeOptionsVisibilityThreshold :: Int
  }

data SpecCommand context m next where
  Before' :: { nodeOptions :: NodeOptions
             , label :: String
             , action :: ExampleT context m ()
             , subspec :: SpecFree context m ()
             , next :: next } -> SpecCommand context m next

  After' :: { nodeOptions :: NodeOptions
            , label :: String
            , action :: ExampleT context m ()
            , subspec :: SpecFree context m ()
            , next :: next } -> SpecCommand context m next

  Introduce' :: { nodeOptions :: NodeOptions
                , label :: String
                , contextLabel :: Label l intro
                , allocate :: ExampleT context m intro
                , cleanup :: intro -> ExampleT context m ()
                , subspecAugmented :: SpecFree (LabelValue l intro :> context) m ()
                , next :: next } -> SpecCommand context m next

  IntroduceWith' :: { nodeOptions :: NodeOptions
                    , label :: String
                    , contextLabel :: Label l intro
                    , introduceAction :: (intro -> ExampleT context m [Result]) -> ExampleT context m ()
                    , subspecAugmented :: SpecFree (LabelValue l intro :> context) m ()
                    , next :: next } -> SpecCommand context m next

  Around' :: { nodeOptions :: NodeOptions
             , label :: String
             , actionWith :: ExampleT context m [Result] -> ExampleT context m ()
             , subspec :: SpecFree context m ()
             , next :: next } -> SpecCommand context m next

  Describe' :: { nodeOptions :: NodeOptions
               , label :: String
               , subspec :: SpecFree context m ()
               , next :: next } -> SpecCommand context m next

  Parallel' :: { nodeOptions :: NodeOptions
               , subspec :: SpecFree context m ()
               , next :: next } -> SpecCommand context m next

  It' :: { nodeOptions :: NodeOptions
         , label :: String
         , example :: ExampleT context m ()
         , next :: next } -> SpecCommand context m next

deriving instance Functor (SpecCommand context m)
deriving instance Foldable (SpecCommand context m)
deriving instance Traversable (SpecCommand context m)


type Spec context m = SpecFree context m ()
type SpecFree context m a = Free (SpecCommand context m) a

makeFree_ ''SpecCommand

instance Show1 (SpecCommand context m) where
  liftShowsPrec sp _ d (Before' {..}) = showsUnaryWith sp [i|Before[#{label}]<#{show subspec}>|] d next
  liftShowsPrec sp _ d (After' {..}) = showsUnaryWith sp [i|After[#{label}]<#{show subspec}>|] d next
  liftShowsPrec sp _ d (Introduce' {..}) = showsUnaryWith sp [i|Introduce[#{label}]<#{show subspecAugmented}>|] d next
  liftShowsPrec sp _ d (IntroduceWith' {..}) = showsUnaryWith sp [i|IntroduceWith[#{label}]<#{show subspecAugmented}>|] d next
  liftShowsPrec sp _ d (Around' {..}) = showsUnaryWith sp [i|Around[#{label}]<#{show subspec}>|] d next
  liftShowsPrec sp _ d (Describe' {..}) = showsUnaryWith sp [i|Describe[#{label}]<#{show subspec}>|] d next
  liftShowsPrec sp _ d (Parallel' {..}) = showsUnaryWith sp [i|Parallel<#{show subspec}>|] d next
  liftShowsPrec sp _ d (It' {..}) = showsUnaryWith sp [i|It[#{label}]|] d next

-- First write beforeEach/afterEach to demonstrate push down approach
-- Then think about how/whether we can to introduceEach / aroundEach


-- | Perform an action before a given spec tree.
before ::
  String
  -- ^ Label for this context manager
  -> ExampleT context m ()
  -- ^ Action to perform
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
before = before' (NodeOptions 100)

-- | Perform an action after a given spec tree.
after ::
  String
  -- ^ Label for this context manager
  -> ExampleT context m ()
  -- ^ Action to perform
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
after = after' (NodeOptions 100)

introduce ::
  String
  -> Label l intro
  -> ExampleT context m intro
  -> (intro -> ExampleT context m ())
  -> SpecFree (LabelValue l intro :> context) m ()
  -> SpecFree context m ()
introduce = introduce' (NodeOptions 100)

introduceWith ::
  String
  -> Label l intro
  -> ((intro -> ExampleT context m [Result]) -> ExampleT context m ())
  -> SpecFree (LabelValue l intro :> context) m ()
  -> SpecFree context m ()
introduceWith = introduceWith' (NodeOptions 100)

around ::
  String
  -> (ExampleT context m [Result] -> ExampleT context m ())
  -> SpecFree context m ()
  -> SpecFree context m ()
around = around' (NodeOptions 100)

-- | Make a group of tests.
describe ::
  String
  -- ^ Label for this group
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
describe = describe' (NodeOptions 50)

-- | Run a group of tests in parallel.
parallel ::
  SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
parallel = parallel' (NodeOptions 50)

-- | Define a single test example.
it ::
  String
  -- ^ Label for the example.
  -> ExampleT context m ()
  -- ^ The test example
  -> Free (SpecCommand context m) ()
it = it' (NodeOptions 0)

beforeEach ::
  String
  -> (ExampleT context m ())
  -> SpecFree context m ()
  -> SpecFree context m ()
beforeEach = beforeEach' (NodeOptions 100)

afterEach ::
  String
  -> (ExampleT context m ())
  -> SpecFree context m ()
  -> SpecFree context m ()
afterEach = afterEach' (NodeOptions 100)

aroundEach :: (Monad m) =>
  String
  -> (ExampleT context m [Result] -> ExampleT context m ())
  -> SpecFree context m ()
  -> SpecFree context m ()
aroundEach = aroundEach' (NodeOptions 100)

-- | Perform an action before each example in a given spec tree.
beforeEach' ::
  NodeOptions
  -> String
  -> (ExampleT context m ())
  -> SpecFree context m ()
  -> SpecFree context m ()
beforeEach' no l f (Free x@(Before' {..})) = Free (x { subspec = beforeEach' no l f subspec, next = beforeEach' no l f next })
beforeEach' no l f (Free x@(After' {..})) = Free (x { subspec = beforeEach' no l f subspec, next = beforeEach' no l f next })
beforeEach' no l f (Free x@(Around' {..})) = Free (x { subspec = beforeEach' no l f subspec, next = beforeEach' no l f next })
beforeEach' no l f (Free x@(Describe' {..})) = Free (x { subspec = beforeEach' no l f subspec, next = beforeEach' no l f next })
beforeEach' no l f (Free x@(Parallel' {..})) = Free (x { subspec = beforeEach' no l f subspec, next = beforeEach' no l f next })
beforeEach' no l f (Free x@(It' {..})) = Free (Before' no l f (Free (x { next = Pure () })) (beforeEach' no l f next))
beforeEach' no l f (Free (Introduce' noi li cl alloc clean subspec next)) = Free (Introduce' noi li cl alloc clean (beforeEach' no l f' subspec) (beforeEach' no l f next))
  where f' = ExampleT $ withReaderT (\(_ :> context) -> context) $ unExampleT f
beforeEach' no l f (Free (IntroduceWith' noi li cl action subspec next)) = Free (IntroduceWith' noi li cl action (beforeEach' no l f' subspec) (beforeEach' no l f next))
  where f' = ExampleT $ withReaderT (\(_ :> context) -> context) $ unExampleT f
beforeEach' _ _ _ (Pure x) = Pure x

-- | Perform an action after each example in a given spec tree.
afterEach' ::
  NodeOptions
  -> String
  -> (ExampleT context m ())
  -> SpecFree context m ()
  -> SpecFree context m ()
afterEach' no l f (Free x@(Before' {..})) = Free (x { subspec = afterEach' no l f subspec, next = afterEach' no l f next })
afterEach' no l f (Free x@(After' {..})) = Free (x { subspec = afterEach' no l f subspec, next = afterEach' no l f next })
afterEach' no l f (Free x@(Around' {..})) = Free (x { subspec = afterEach' no l f subspec, next = afterEach' no l f next })
afterEach' no l f (Free x@(Describe' {..})) = Free (x { subspec = afterEach' no l f subspec, next = afterEach' no l f next })
afterEach' no l f (Free x@(Parallel' {..})) = Free (x { subspec = afterEach' no l f subspec, next = afterEach' no l f next })
afterEach' no l f (Free x@(It' {..})) = Free (After' no l f (Free (x { next = Pure () })) (afterEach' no l f next))
afterEach' no l f (Free (Introduce' noi li cl alloc clean subspec next)) = Free (Introduce' noi li cl alloc clean (afterEach' no l f' subspec) (afterEach' no l f next))
  where f' = ExampleT $ withReaderT (\(_ :> context) -> context) $ unExampleT f
afterEach' no l f (Free (IntroduceWith' noi li cl action subspec next)) = Free (IntroduceWith' noi li cl action (afterEach' no l f' subspec) (afterEach' no l f next))
  where f' = ExampleT $ withReaderT (\(_ :> context) -> context) $ unExampleT f
afterEach' _ _ _ (Pure x) = Pure x

aroundEach' :: (Monad m) =>
  NodeOptions
  -> String
  -> (ExampleT context m [Result] -> ExampleT context m ())
  -> SpecFree context m ()
  -> SpecFree context m ()
aroundEach' no l f (Free x@(Before' {..})) = Free (x { subspec = aroundEach' no l f subspec, next = aroundEach' no l f next })
aroundEach' no l f (Free x@(After' {..})) = Free (x { subspec = aroundEach' no l f subspec, next = aroundEach' no l f next })
aroundEach' no l f (Free x@(Around' {..})) = Free (x { subspec = aroundEach' no l f subspec, next = aroundEach' no l f next })
aroundEach' no l f (Free x@(Describe' {..})) = Free (x { subspec = aroundEach' no l f subspec, next = aroundEach' no l f next })
aroundEach' no l f (Free x@(Parallel' {..})) = Free (x { subspec = aroundEach' no l f subspec, next = aroundEach' no l f next })
aroundEach' no l f (Free x@(It' {..})) = Free (Around' no l f (Free (x { next = Pure () })) (aroundEach' no l f next))
aroundEach' no _ _ (Pure x) = Pure x
aroundEach' no l f (Free (IntroduceWith' noi li cl action subspec next)) = Free (IntroduceWith' noi li cl action (aroundEach' no l (unwrapContext f) subspec) (aroundEach' no l f next))
aroundEach' no l f (Free (Introduce' noi li cl alloc clean subspec next)) = Free (Introduce' noi li cl alloc clean (aroundEach' no l (unwrapContext f) subspec) (aroundEach' no l f next))

unwrapContext :: forall m introduce context. (Monad m) => (ExampleT context m [Result] -> ExampleT context m ()) -> ExampleT (introduce :> context) m [Result] -> ExampleT (introduce :> context) m ()
unwrapContext f (ExampleT action) = do
  i :> c <- ask
  ExampleT $ withReaderT (\(_ :> context) -> context) $ unExampleT $ f $ ExampleT (withReaderT (i :>) action)
