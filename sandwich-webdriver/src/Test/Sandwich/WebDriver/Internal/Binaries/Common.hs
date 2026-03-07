
module Test.Sandwich.WebDriver.Internal.Binaries.Common where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import qualified Data.Text as T
import System.Exit
import System.FilePath
import Test.Sandwich.Expectations
import Test.Sandwich.Logging
import Test.Sandwich.Misc (HasBaseContextMonad)
import Test.Sandwich.WebDriver.Internal.Util
import UnliftIO.Async
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.Process
import UnliftIO.Temporary


downloadAndUnzipToPath :: (MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m) => T.Text -> FilePath -> m (Either T.Text ())
downloadAndUnzipToPath downloadPath localPath = leftOnException' $ do
  info [i|Downloading #{downloadPath} to #{localPath}|]
  createDirectoryIfMissing True (takeDirectory localPath)
  withSystemTempDirectory "sandwich-webdriver-tool-download" $ \dir -> do
    curlDownloadToPath (T.unpack downloadPath) (dir </> "temp.zip")

    createProcessWithLogging ((proc "unzip" ["temp.zip", "-d", "unzipped"]) { cwd = Just dir })
      >>= \(ps, asy) -> finally (waitForProcess ps >>= (`shouldBe` ExitSuccess))
                                (cancel asy)
    let unzipped = dir </> "unzipped"

    executables <- (filter (/= "") . T.splitOn "\n" . T.pack) <$> readCreateProcessWithLogging (proc "find" [unzipped, "-executable", "-type", "f"]) ""
    case executables of
      [] -> throwIO $ userError [i|No executable found in file downloaded from #{downloadPath}|]
      [x] -> do
        copyFile (T.unpack x) localPath
        createProcessWithLogging (shell [i|chmod u+x #{localPath}|])
          >>= \(ps, asy) -> finally (waitForProcess ps >>= (`shouldBe` ExitSuccess))
                                    (cancel asy)
      xs -> throwIO $ userError [i|Found multiple executable found in file downloaded from #{downloadPath}: #{xs}|]

downloadAndUntarballToPath :: (MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m) => T.Text -> FilePath -> m (Either T.Text ())
downloadAndUntarballToPath downloadPath localPath = leftOnException' $ do
  info [i|Downloading #{downloadPath} to #{localPath}|]
  createDirectoryIfMissing True (takeDirectory localPath)
  createProcessWithLogging (shell [i|wget -qO- #{downloadPath} | tar xvz  -C #{takeDirectory localPath}|])
    >>= \(ps, asy) -> finally (liftIO $ waitForProcess ps >>= (`shouldBe` ExitSuccess))
                              (cancel asy)
  createProcessWithLogging (shell [i|chmod u+x #{localPath}|])
    >>= \(ps, asy) -> finally (liftIO $ waitForProcess ps >>= (`shouldBe` ExitSuccess))
                              (cancel asy)

curlDownloadToPath :: (MonadUnliftIO m, MonadLogger m, HasBaseContextMonad context m) => String -> FilePath -> m ()
curlDownloadToPath downloadPath localPath = do
  info [i|Downloading #{downloadPath} to #{localPath}|]
  createDirectoryIfMissing True (takeDirectory localPath)
  (p, asy) <- createProcessWithLogging (proc "curl" [downloadPath, "-o", localPath, "-s"])
  finally (liftIO (waitForProcess p) >>= (`shouldBe` ExitSuccess))
          (cancel asy)

unlessM :: Monad m => m Bool -> m () -> m ()
unlessM b s = b >>= (\t -> unless t s)
