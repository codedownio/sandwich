
module Common where

import Control.Concurrent
import Control.Monad.Catch
import Control.Monad.IO.Class
import System.Random
import Test.Sandwich


pauseSeconds :: (MonadIO m) => Double -> m ()
pauseSeconds s = liftIO $ threadDelay $ round (s * 1000000)

pauseRandomAndSucceed :: (MonadIO m) => ExampleT context m ()
pauseRandomAndSucceed = pauseRandom >> 2 `shouldBe` 2

pauseRandomAndFail :: (MonadIO m) => ExampleT context m ()
pauseRandomAndFail = pauseRandom >> 2 `shouldBe` 3

pauseRandom :: (MonadIO m) => m ()
pauseRandom = liftIO $ do
  pauseTime <- randomRIO (1000000, 4000000)
  threadDelay pauseTime
