{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

-- | The core Spec/SpecCommand types, used to define the test free monad.

module Test.Sandwich.Types.Spec where

import Control.Exception.Safe
import Control.Monad.Base
import Control.Monad.Except
import Control.Monad.Free
import Control.Monad.Free.TH
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Functor.Classes
import Data.String.Interpolate
import GHC.Stack
import GHC.TypeLits
import Safe

-- * ExampleM monad

newtype ExampleT context m a = ExampleT { unExampleT :: ReaderT context (LoggingT m) a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader context, MonadLogger, MonadLoggerIO, MonadThrow, MonadCatch, MonadMask)
type ExampleM context = ExampleT context IO

instance (MonadIO m, MonadUnliftIO m) => MonadUnliftIO (ExampleT context m) where
  withRunInIO inner = ExampleT $ withRunInIO $ \run -> inner (run . unExampleT)

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

instance (Monad m, MonadThrow m) => MonadFail (ExampleT context m) where
  fail :: (HasCallStack) => String -> ExampleT context m a
  fail = throwIO . Reason (Just callStack)

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

-- | Options for an individual test node.
data NodeOptions = NodeOptions {
  nodeOptionsVisibilityThreshold :: Int
  -- ^ The visibility threshold of the node. See the main docs for an explanation of this.
  , nodeOptionsCreateFolder :: Bool
  -- ^ Whether to create a folder in the on-disk test results for this node.
  -- Defaults to 'True', but can be turned off to reduce extraneous folders from nodes like 'Parallel'.
  , nodeOptionsRecordTime :: Bool
  -- ^ Whether to time this node.
  }

-- | Reasonable default node options.
defaultNodeOptions :: NodeOptions
defaultNodeOptions = NodeOptions 100 True True

data SpecCommand context m next where
  Before'' :: { location :: Maybe SrcLoc
              , nodeOptions :: NodeOptions
              , label :: String
              , action :: ExampleT context m ()
              , subspec :: SpecFree context m ()
              , next :: next } -> SpecCommand context m next

  After'' :: { location :: Maybe SrcLoc
             , nodeOptions :: NodeOptions
             , label :: String
             , action :: ExampleT context m ()
             , subspec :: SpecFree context m ()
             , next :: next } -> SpecCommand context m next

  Introduce'' :: { location :: Maybe SrcLoc
                 , nodeOptions :: NodeOptions
                 , label :: String
                 , contextLabel :: Label l intro
                 , allocate :: ExampleT context m intro
                 , cleanup :: intro -> ExampleT context m ()
                 , subspecAugmented :: SpecFree (LabelValue l intro :> context) m ()
                 , next :: next } -> SpecCommand context m next

  IntroduceWith'' :: { location :: Maybe SrcLoc
                     , nodeOptions :: NodeOptions
                     , label :: String
                     , contextLabel :: Label l intro
                     , introduceAction :: (intro -> ExampleT context m [Result]) -> ExampleT context m ()
                     , subspecAugmented :: SpecFree (LabelValue l intro :> context) m ()
                     , next :: next } -> SpecCommand context m next

  Around'' :: { location :: Maybe SrcLoc
              , nodeOptions :: NodeOptions
              , label :: String
              , actionWith :: ExampleT context m [Result] -> ExampleT context m ()
              , subspec :: SpecFree context m ()
              , next :: next } -> SpecCommand context m next

  Describe'' :: { location :: Maybe SrcLoc
                , nodeOptions :: NodeOptions
                , label :: String
                , subspec :: SpecFree context m ()
                , next :: next } -> SpecCommand context m next

  Parallel'' :: { location :: Maybe SrcLoc
                , nodeOptions :: NodeOptions
                , subspec :: SpecFree context m ()
                , next :: next } -> SpecCommand context m next

  It'' :: { location :: Maybe SrcLoc
          , nodeOptions :: NodeOptions
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
  liftShowsPrec sp _ d (Before'' {..}) = showsUnaryWith sp [i|Before[#{label}]<#{show subspec}>|] d next
  liftShowsPrec sp _ d (After'' {..}) = showsUnaryWith sp [i|After[#{label}]<#{show subspec}>|] d next
  liftShowsPrec sp _ d (Introduce'' {..}) = showsUnaryWith sp [i|Introduce[#{label}]<#{show subspecAugmented}>|] d next
  liftShowsPrec sp _ d (IntroduceWith'' {..}) = showsUnaryWith sp [i|IntroduceWith[#{label}]<#{show subspecAugmented}>|] d next
  liftShowsPrec sp _ d (Around'' {..}) = showsUnaryWith sp [i|Around[#{label}]<#{show subspec}>|] d next
  liftShowsPrec sp _ d (Describe'' {..}) = showsUnaryWith sp [i|Describe[#{label}]<#{show subspec}>|] d next
  liftShowsPrec sp _ d (Parallel'' {..}) = showsUnaryWith sp [i|Parallel<#{show subspec}>|] d next
  liftShowsPrec sp _ d (It'' {..}) = showsUnaryWith sp [i|It[#{label}]|] d next

-- First write beforeEach/afterEach to demonstrate push down approach
-- Then think about how/whether we can to introduceEach / aroundEach

-- * ----------------------------------------------------------

-- | Perform an action before a given spec tree.
before :: (HasCallStack) =>
  String
  -- ^ Label for this context manager
  -> ExampleT context m ()
  -- ^ Action to perform
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
before = before' (defaultNodeOptions { nodeOptionsVisibilityThreshold = 100 })

-- | Perform an action before a given spec tree.
before' :: (HasCallStack) =>
  NodeOptions
  -- ^ Custom options for this node
  -> String
  -- ^ String label for this context manager
  -> ExampleT context m ()
  -- ^ Action to perform
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
before' = before'' (snd <$> headMay (drop 1 $ getCallStack callStack))

-- | Perform an action before a given spec tree.
before'' :: (HasCallStack) =>
  Maybe SrcLoc
  -- ^ Location of this call
  -> NodeOptions
  -- ^ Custom options for this node
  -> String
  -- ^ String label for this context manager
  -> ExampleT context m ()
  -- ^ Action to perform
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()

-- * ----------------------------------------------------------

-- | Perform an action after a given spec tree.
after :: (HasCallStack) =>
  String
  -- ^ Label for this context manager
  -> ExampleT context m ()
  -- ^ Action to perform
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
after = after' (defaultNodeOptions { nodeOptionsVisibilityThreshold = 100 })

-- | Perform an action after a given spec tree.
after' :: (HasCallStack) =>
  NodeOptions
  -- ^ Custom options for this node
  -> String
  -- ^ String label for this context manager
  -> ExampleT context m ()
  -- ^ Action to perform
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
after' = after'' (snd <$> headMay (drop 1 $ getCallStack callStack))

-- | Perform an action after a given spec tree.
after'' :: (HasCallStack) =>
  Maybe SrcLoc
  -- ^ Location of this call
  -> NodeOptions
  -- ^ Custom options for this node
  -> String
  -- ^ String label for this context manager
  -> ExampleT context m ()
  -- ^ Action to perform
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()

-- * ----------------------------------------------------------

-- | Introduce a new value and make it available to the child spec tree.
introduce :: (HasCallStack) =>
  String
  -- ^ String label for this node
  -> Label l intro
  -- ^ 'Label' under which to introduce the value
  -> ExampleT context m intro
  -- ^ Action to produce the new value (of type 'intro')
  -> (intro -> ExampleT context m ())
  -- ^ Action to clean up the new value
  -> SpecFree (LabelValue l intro :> context) m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
introduce = introduce' (defaultNodeOptions { nodeOptionsVisibilityThreshold = 100 })

-- | Introduce a new value and make it available to the child spec tree.
introduce' :: (HasCallStack) =>
  NodeOptions
  -- ^ Custom options for this node
  -> String
  -- ^ String label for this node
  -> Label l intro
  -- ^ 'Label' under which to introduce the value
  -> ExampleT context m intro
  -- ^ Action to produce the new value (of type 'intro')
  -> (intro -> ExampleT context m ())
  -- ^ Action to clean up the new value
  -> SpecFree (LabelValue l intro :> context) m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
introduce' = introduce'' (snd <$> headMay (drop 1 $ getCallStack callStack))

-- | Introduce a new value and make it available to the child spec tree.
introduce'' :: (HasCallStack) =>
  Maybe SrcLoc
  -- ^ Location of this call
  -> NodeOptions
  -- ^ Custom options for this node
  -> String
  -- ^ String label for this node
  -> Label l intro
  -- ^ 'Label' under which to introduce the value
  -> ExampleT context m intro
  -- ^ Action to produce the new value (of type 'intro')
  -> (intro -> ExampleT context m ())
  -- ^ Action to clean up the new value
  -> SpecFree (LabelValue l intro :> context) m ()
  -- ^ Child spec tree
  -> SpecFree context m ()

-- * ----------------------------------------------------------

-- | Introduce a new value in an 'around' fashion, so it can be used with context managers like withFile or bracket.
introduceWith :: (HasCallStack) =>
  String
  -- ^ String label for this node
  -> Label l intro
  -- ^ 'Label' under which to introduce the value
  -> ((intro -> ExampleT context m [Result]) -> ExampleT context m ())
  -- ^ Callback to receive the new value and the child tree.
  -> SpecFree (LabelValue l intro :> context) m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
introduceWith = introduceWith' (defaultNodeOptions { nodeOptionsVisibilityThreshold = 100 })

-- | Introduce a new value in an 'around' fashion, so it can be used with context managers like withFile or bracket.
introduceWith' :: (HasCallStack) =>
  NodeOptions
  -- ^ Custom options for this node
  -> String
  -- ^ String label for this node
  -> Label l intro
  -- ^ 'Label' under which to introduce the value
  -> ((intro -> ExampleT context m [Result]) -> ExampleT context m ())
  -- ^ Callback to receive the new value and the child tree.
  -> SpecFree (LabelValue l intro :> context) m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
introduceWith' = introduceWith'' (snd <$> headMay (drop 1 $ getCallStack callStack))

-- | Introduce a new value in an 'around' fashion, so it can be used with context managers like withFile or bracket.
introduceWith'' :: (HasCallStack) =>
  Maybe SrcLoc
  -- ^ Location of this call
  -> NodeOptions
  -- ^ Custom options for this node
  -> String
  -- ^ String label for this node
  -> Label l intro
  -- ^ 'Label' under which to introduce the value
  -> ((intro -> ExampleT context m [Result]) -> ExampleT context m ())
  -- ^ Callback to receive the new value and the child tree.
  -> SpecFree (LabelValue l intro :> context) m ()
  -- ^ Child spec tree
  -> SpecFree context m ()

-- * ----------------------------------------------------------

-- | Run an action around the given child subtree. Useful for context managers like withFile or bracket.
around :: (HasCallStack) =>
  String
  -> (ExampleT context m [Result] -> ExampleT context m ())
  -- ^ Callback to run the child tree
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
around = around' (defaultNodeOptions { nodeOptionsVisibilityThreshold = 100 })

-- | Run an action around the given child subtree. Useful for context managers like withFile or bracket.
around' :: (HasCallStack) =>
  NodeOptions
  -- ^ Custom options for this node
  -> String
  -- ^ String label for this node
  -> (ExampleT context m [Result] -> ExampleT context m ())
  -- ^ Callback to run the child tree
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
around' = around'' (snd <$> headMay (drop 1 $ getCallStack callStack))

-- | Run an action around the given child subtree. Useful for context managers like withFile or bracket.
around'' :: (HasCallStack) =>
  Maybe SrcLoc
  -- ^ Location of this call
  -> NodeOptions
  -- ^ Custom options for this node
  -> String
  -- ^ String label for this node
  -> (ExampleT context m [Result] -> ExampleT context m ())
  -- ^ Callback to run the child tree
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()


-- * ----------------------------------------------------------

-- | Make a group of tests.
describe :: (HasCallStack) =>
  String
  -- ^ Label for this group
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
describe = describe' (defaultNodeOptions { nodeOptionsVisibilityThreshold = 50 })

-- | Make a group of tests.
describe' :: (HasCallStack) =>
  NodeOptions
  -- ^ Custom options for this node
  -> String
  -- ^ Label for this group
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
describe' = describe'' (snd <$> headMay (drop 1 $ getCallStack callStack))

-- | Make a group of tests.
describe'' :: (HasCallStack) =>
  Maybe SrcLoc
  -- ^ Location of this call
  -> NodeOptions
  -- ^ Custom options for this node
  -> String
  -- ^ Label for this group
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()

-- * ----------------------------------------------------------

-- | Run a group of tests in parallel.
parallel :: (HasCallStack) =>
  SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
parallel = parallel' (defaultNodeOptions { nodeOptionsVisibilityThreshold = 70 })

-- | Run a group of tests in parallel.
parallel' :: (HasCallStack) =>
  NodeOptions
  -- ^ Custom options for this node
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
parallel' = parallel'' (snd <$> headMay (drop 1 $ getCallStack callStack))

-- | Run a group of tests in parallel.
parallel'' :: (HasCallStack) =>
  Maybe SrcLoc
  -- ^ Location of this call
  -> NodeOptions
  -- ^ Custom options for this node
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()

-- * ----------------------------------------------------------

-- | Define a single test example.
it :: (HasCallStack) =>
  String
  -- ^ Label for the example.
  -> ExampleT context m ()
  -- ^ The test example
  -> Free (SpecCommand context m) ()
it = it' (defaultNodeOptions { nodeOptionsVisibilityThreshold = 0 })

-- | Define a single test example.
it' :: (HasCallStack) =>
  NodeOptions
  -- ^ Custom options for this node
  -> String
  -- ^ String label for the example.
  -> ExampleT context m ()
  -- ^ The test example
  -> Free (SpecCommand context m) ()
it' = it'' (snd <$> headMay (drop 1 $ getCallStack callStack))

-- | Define a single test example.
it'' :: (HasCallStack) =>
  Maybe SrcLoc
  -- ^ Location of this call
  -> NodeOptions
  -- ^ Custom options for this node
  -> String
  -- ^ String label for the example.
  -> ExampleT context m ()
  -- ^ The test example
  -> Free (SpecCommand context m) ()

-- * ----------------------------------------------------------

-- | Same as 'before', but applied individually to every 'it' node.
beforeEach :: (HasCallStack) =>
  String
  -- ^ String label for this context manager
  -> (ExampleT context m ())
  -- ^ Action to perform
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
beforeEach = beforeEach' (snd <$> headMay (getCallStack callStack)) (defaultNodeOptions { nodeOptionsVisibilityThreshold = 100 })

-- | Perform an action before each example in a given spec tree.
beforeEach' :: (HasCallStack) =>
  Maybe SrcLoc
  -- ^ Location of this call
  -> NodeOptions
  -- ^ Custom options for this node
  -> String
  -- ^ String label for this context manager
  -> (ExampleT context m ())
  -- ^ Action to perform
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
beforeEach' loc no l f (Free x@(Before'' {..})) = Free (x { subspec = beforeEach' loc no l f subspec, next = beforeEach' loc no l f next })
beforeEach' loc no l f (Free x@(After'' {..})) = Free (x { subspec = beforeEach' loc no l f subspec, next = beforeEach' loc no l f next })
beforeEach' loc no l f (Free x@(Around'' {..})) = Free (x { subspec = beforeEach' loc no l f subspec, next = beforeEach' loc no l f next })
beforeEach' loc no l f (Free x@(Describe'' {..})) = Free (x { subspec = beforeEach' loc no l f subspec, next = beforeEach' loc no l f next })
beforeEach' loc no l f (Free x@(Parallel'' {..})) = Free (x { subspec = beforeEach' loc no l f subspec, next = beforeEach' loc no l f next })
beforeEach' loc no l f (Free x@(It'' {..})) = Free (Before'' loc no l f (Free (x { next = Pure () })) (beforeEach' loc no l f next))
beforeEach' loc no l f (Free (Introduce'' loci noi li cl alloc clean subspec next)) = Free (Introduce'' loci noi li cl alloc clean (beforeEach' loc no l f' subspec) (beforeEach' loc no l f next))
  where f' = ExampleT $ withReaderT (\(_ :> context) -> context) $ unExampleT f
beforeEach' loc no l f (Free (IntroduceWith'' loci noi li cl action subspec next)) = Free (IntroduceWith'' loci noi li cl action (beforeEach' loc no l f' subspec) (beforeEach' loc no l f next))
  where f' = ExampleT $ withReaderT (\(_ :> context) -> context) $ unExampleT f
beforeEach' _ _ _ _ (Pure x) = Pure x

-- * ----------------------------------------------------------

-- | Same as 'after', but applied individually to every 'it' node.
afterEach :: (HasCallStack) =>
  String
  -- ^ String label for this context manager
  -> (ExampleT context m ())
  -- ^ Action to perform
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
afterEach = afterEach' (snd <$> headMay (getCallStack callStack)) (defaultNodeOptions { nodeOptionsVisibilityThreshold = 100 })

-- | Perform an action after each example in a given spec tree.
afterEach' :: (HasCallStack) =>
  Maybe SrcLoc
  -- ^ Location of this call
  -> NodeOptions
  -- ^ Custom options for this node
  -> String
  -- ^ Label for this context manager
  -> (ExampleT context m ())
  -- ^ Action to perform
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
afterEach' loc no l f (Free x@(Before'' {..})) = Free (x { subspec = afterEach' loc no l f subspec, next = afterEach' loc no l f next })
afterEach' loc no l f (Free x@(After'' {..})) = Free (x { subspec = afterEach' loc no l f subspec, next = afterEach' loc no l f next })
afterEach' loc no l f (Free x@(Around'' {..})) = Free (x { subspec = afterEach' loc no l f subspec, next = afterEach' loc no l f next })
afterEach' loc no l f (Free x@(Describe'' {..})) = Free (x { subspec = afterEach' loc no l f subspec, next = afterEach' loc no l f next })
afterEach' loc no l f (Free x@(Parallel'' {..})) = Free (x { subspec = afterEach' loc no l f subspec, next = afterEach' loc no l f next })
afterEach' loc no l f (Free x@(It'' {..})) = Free (After'' loc no l f (Free (x { next = Pure () })) (afterEach' loc no l f next))
afterEach' loc no l f (Free (Introduce'' loci noi li cl alloc clean subspec next)) = Free (Introduce'' loci noi li cl alloc clean (afterEach' loc no l f' subspec) (afterEach' loc no l f next))
  where f' = ExampleT $ withReaderT (\(_ :> context) -> context) $ unExampleT f
afterEach' loc no l f (Free (IntroduceWith'' loci noi li cl action subspec next)) = Free (IntroduceWith'' loci noi li cl action (afterEach' loc no l f' subspec) (afterEach' loc no l f next))
  where f' = ExampleT $ withReaderT (\(_ :> context) -> context) $ unExampleT f
afterEach' _ _ _ _ (Pure x) = Pure x

-- * ----------------------------------------------------------

-- | Same as 'around', but applied individually to every 'it' node.
aroundEach :: (Monad m, HasCallStack) =>
  String
  -- ^ String label for this context manager
  -> (ExampleT context m [Result] -> ExampleT context m ())
  -- ^ Callback to run the child tree
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
aroundEach = aroundEach' (snd <$> headMay (getCallStack callStack)) (defaultNodeOptions { nodeOptionsVisibilityThreshold = 100 })

aroundEach' :: (Monad m, HasCallStack) =>
  Maybe SrcLoc
  -- ^ Location of this call
  -> NodeOptions
  -- ^ Custom options for this node
  -> String
  -- ^ String label for this context manager
  -> (ExampleT context m [Result] -> ExampleT context m ())
  -- ^ Callback to run the child tree
  -> SpecFree context m ()
  -- ^ Child spec tree
  -> SpecFree context m ()
aroundEach' loc no l f (Free x@(Before'' {..})) = Free (x { subspec = aroundEach' loc no l f subspec, next = aroundEach' loc no l f next })
aroundEach' loc no l f (Free x@(After'' {..})) = Free (x { subspec = aroundEach' loc no l f subspec, next = aroundEach' loc no l f next })
aroundEach' loc no l f (Free x@(Around'' {..})) = Free (x { subspec = aroundEach' loc no l f subspec, next = aroundEach' loc no l f next })
aroundEach' loc no l f (Free x@(Describe'' {..})) = Free (x { subspec = aroundEach' loc no l f subspec, next = aroundEach' loc no l f next })
aroundEach' loc no l f (Free x@(Parallel'' {..})) = Free (x { subspec = aroundEach' loc no l f subspec, next = aroundEach' loc no l f next })
aroundEach' loc no l f (Free x@(It'' {..})) = Free (Around'' loc no l f (Free (x { next = Pure () })) (aroundEach' loc no l f next))
aroundEach' _no _ _ _ (Pure x) = Pure x
aroundEach' loc no l f (Free (IntroduceWith'' loci noi li cl action subspec next)) = Free (IntroduceWith'' loci noi li cl action (aroundEach' loc no l (unwrapContext f) subspec) (aroundEach' loc no l f next))
aroundEach' loc no l f (Free (Introduce'' loci noi li cl alloc clean subspec next)) = Free (Introduce'' loci noi li cl alloc clean (aroundEach' loc no l (unwrapContext f) subspec) (aroundEach' loc no l f next))

-- * ----------------------------------------------------------

unwrapContext :: forall m introduce context. (Monad m) => (ExampleT context m [Result] -> ExampleT context m ()) -> ExampleT (introduce :> context) m [Result] -> ExampleT (introduce :> context) m ()
unwrapContext f (ExampleT action) = do
  i :> _ <- ask
  ExampleT $ withReaderT (\(_ :> context) -> context) $ unExampleT $ f $ ExampleT (withReaderT (i :>) action)
