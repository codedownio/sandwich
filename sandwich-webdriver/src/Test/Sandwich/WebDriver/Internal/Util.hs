{-# LANGUAGE CPP #-}

module Test.Sandwich.WebDriver.Internal.Util where

import Control.Monad
import Control.Monad.IO.Unlift
import qualified Data.Text as T
import qualified System.Random as R
import Test.Sandwich (expectationFailure)
import UnliftIO.Exception

#ifdef mingw32_HOST_OS
import System.IO
#endif


-- * Exceptions

leftOnException :: (MonadUnliftIO m) => m (Either T.Text a) -> m (Either T.Text a)
leftOnException = handle (\(e :: SomeException) -> return $ Left $ T.pack $ show e)

exceptionOnLeft :: (MonadUnliftIO m) => m (Either T.Text a) -> m a
exceptionOnLeft action = action >>= \case
  Left err -> expectationFailure (T.unpack err)
  Right x -> pure x

leftOnException' :: (MonadUnliftIO m) => m a -> m (Either T.Text a)
leftOnException' action = catch (Right <$> action) (\(e :: SomeException) -> return $ Left $ T.pack $ show e)

-- * Util

whenJust :: (Monad m) => Maybe a -> (a -> m b) -> m ()
whenJust Nothing _ = return ()
whenJust (Just x) action = void $ action x

whenLeft :: (Monad m) => Either a b -> (a -> m ()) -> m ()
whenLeft (Left x) action = action x
whenLeft (Right _) _ = return ()

whenRight :: (Monad m) => Either a b -> (b -> m ()) -> m ()
whenRight (Left _) _ = return ()
whenRight (Right x) action = action x

makeUUID :: IO T.Text
makeUUID = (T.pack . take 10 . R.randomRs ('a','z')) <$> R.newStdGen
