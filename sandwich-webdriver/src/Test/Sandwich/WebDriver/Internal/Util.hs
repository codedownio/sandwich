{-# LANGUAGE CPP #-}

module Test.Sandwich.WebDriver.Internal.Util where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Trans.Control (MonadBaseControl)
import Data.String.Interpolate
import qualified Data.Text as T
import System.Directory
import System.Process
import qualified System.Random as R
import UnliftIO.Exception

#ifdef mingw32_HOST_OS
import System.IO
#endif


-- * Truncating log files

moveAndTruncate :: FilePath -> String -> IO ()
moveAndTruncate from to = do
  exists <- doesFileExist from
  when exists $ do
    copyFile from to
    tryTruncateFile from

  where
    tryTruncateFile :: FilePath -> IO ()
    tryTruncateFile path = catch (truncateFile path)
                                 (\(e :: SomeException) -> putStrLn [i|Failed to truncate file #{path}: #{e}|])

    truncateFile :: FilePath -> IO ()
#ifdef mingw32_HOST_OS
    truncateFile path = withFile path WriteMode $ flip hPutStr "\n" -- Not exactly truncation, but close enough?
#else
    truncateFile path = void $ readCreateProcess (shell [i|> #{path}|]) ""
#endif

-- * Exceptions

leftOnException :: (MonadUnliftIO m) => m (Either T.Text a) -> m (Either T.Text a)
leftOnException = handle (\(e :: SomeException) -> return $ Left $ T.pack $ show e)

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
