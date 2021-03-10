{-# LANGUAGE RankNTypes, MultiWayIf, ScopedTypeVariables, CPP, QuasiQuotes, RecordWildCards #-}
-- |

module Test.Sandwich.WebDriver.Internal.Screenshots where

import Control.Concurrent
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.IO.Class
import qualified Data.Map as M
import Data.String.Interpolate
import qualified Data.Text as T
import GHC.Stack
import Network.HTTP.Client
import System.FilePath
import Test.Sandwich.WebDriver.Internal.Types
import Test.WebDriver

saveScreenshots :: (HasCallStack) => T.Text -> WebDriver -> FilePath -> IO ()
saveScreenshots screenshotName (WebDriver {..}) resultsDir = do
  -- For every session, and for every window, try to get a screenshot for the results dir
  sessionMap <- readMVar wdSessionMap
  forM_ (M.toList sessionMap) $ \(browser, sess) -> runWD sess $
    handle (\(e :: HttpException) -> case e of
               (HttpExceptionRequest _ content) -> liftIO $ putStrLn [i|HttpException when trying to take a screenshot: '#{content}'|]
               e -> liftIO $ putStrLn [i|HttpException when trying to take a screenshot: '#{e}'|])
           (saveScreenshot $ resultsDir </> [i|#{browser}_#{screenshotName}.png|])
