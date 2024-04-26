
module Sandwich.Contexts.Kubernetes.Util.Exception where

import Control.Monad.IO.Unlift
import Relude
import Test.Sandwich.Misc
import UnliftIO.Exception


leftOnException :: (MonadUnliftIO m) => m (Either Text a) -> m (Either Text a)
leftOnException = handleAny $ \e -> return $ Left $ case fromException e of
  Just (Reason _ msg) -> toText msg
  _ -> show e
