{-# LANGUAGE RankNTypes #-}
-- |

module Test.Sandwich.Types.Util where

import Control.Monad.Trans.Control

-- * Machinery needed to write MonadTransControl/MonadBaseControl instances for our main free monad

type RunDefault3 t n n' n'' = forall m b. (Monad m, Monad (n'' m), Monad (n' (n'' m))) => t m b -> m (StT n'' (StT n' (StT n b)))

defaultLiftWith3 :: (Monad m, Monad (n'' m), Monad (n' (n'' m)), MonadTransControl n, MonadTransControl n', MonadTransControl n'')
                 => (forall b.   n (n' (n'' m)) b -> t m b)     -- ^ Monad constructor
                 -> (forall o b. t o b -> n (n' (n'' o)) b)     -- ^ Monad deconstructor
                 -> (RunDefault3 t n n' n'' -> m a)
                 -> t m a
defaultLiftWith3 t unT = \f -> t $
  liftWith $ \run ->
  liftWith $ \run' ->
  liftWith $ \run'' ->
  f $ run'' . run' . run . unT
{-# INLINABLE defaultLiftWith3 #-}

defaultRestoreT3 :: (Monad m, Monad (n'' m), Monad (n' (n'' m)), MonadTransControl n, MonadTransControl n', MonadTransControl n'')
                 => (n (n' (n'' m)) a -> t m a)     -- ^ Monad constructor
                 -> m (StT n'' (StT n' (StT n a)))
                 -> t m a
defaultRestoreT3 t = t . restoreT . restoreT . restoreT
{-# INLINABLE defaultRestoreT3 #-}
