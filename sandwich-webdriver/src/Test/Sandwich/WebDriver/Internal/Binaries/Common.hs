
module Test.Sandwich.WebDriver.Internal.Binaries.Common where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import qualified Data.Text as T
import System.Directory
import System.Exit
import System.FilePath
import System.Process
import Test.Sandwich.Expectations
import Test.Sandwich.Logging
import Test.Sandwich.WebDriver.Internal.Util
import UnliftIO.Temporary


downloadAndUnzipToPath :: (MonadUnliftIO m, MonadLogger m) => T.Text -> FilePath -> m (Either T.Text ())
downloadAndUnzipToPath downloadPath localPath = leftOnException' $ do
  info [i|Downloading #{downloadPath} to #{localPath}|]
  liftIO $ createDirectoryIfMissing True (takeDirectory localPath)
  withSystemTempDirectory "sandwich-webdriver-tool-download" $ \dir -> do
    curlDownloadToPath (T.unpack downloadPath) (dir </> "temp.zip")

    createProcessWithLogging ((proc "unzip" ["temp.zip", "-d", "unzipped"]) { cwd = Just dir })
      >>= liftIO . waitForProcess >>= (`shouldBe` ExitSuccess)
    let unzipped = dir </> "unzipped"

    executables <- (filter (/= "") . T.splitOn "\n" . T.pack) <$> readCreateProcessWithLogging (proc "find" [unzipped, "-executable", "-type", "f"]) ""
    case executables of
      [] -> liftIO $ throwIO $ userError [i|No executable found in file downloaded from #{downloadPath}|]
      [x] -> do
        liftIO $ copyFile (T.unpack x) localPath
        createProcessWithLogging (shell [i|chmod u+x #{localPath}|])
          >>= liftIO . waitForProcess >>= (`shouldBe` ExitSuccess)
      xs -> liftIO $ throwIO $ userError [i|Found multiple executable found in file downloaded from #{downloadPath}: #{xs}|]

downloadAndUntarballToPath :: (MonadUnliftIO m, MonadLogger m) => T.Text -> FilePath -> m (Either T.Text ())
downloadAndUntarballToPath downloadPath localPath = leftOnException' $ do
  info [i|Downloading #{downloadPath} to #{localPath}|]
  liftIO $ createDirectoryIfMissing True (takeDirectory localPath)
  createProcessWithLogging (shell [i|wget -qO- #{downloadPath} | tar xvz  -C #{takeDirectory localPath}|])
    >>= liftIO . waitForProcess >>= (`shouldBe` ExitSuccess)
  createProcessWithLogging (shell [i|chmod u+x #{localPath}|])
    >>= liftIO . waitForProcess >>= (`shouldBe` ExitSuccess)

curlDownloadToPath :: (MonadUnliftIO m, MonadLogger m) => String -> FilePath -> m ()
curlDownloadToPath downloadPath localPath = do
  info [i|Downloading #{downloadPath} to #{localPath}|]
  liftIO $ createDirectoryIfMissing True (takeDirectory localPath)
  p <- createProcessWithLogging (proc "curl" [downloadPath, "-o", localPath, "-s"])
  liftIO (waitForProcess p) >>= (`shouldBe` ExitSuccess)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b s = b >>= (\t -> unless t s)
