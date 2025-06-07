
module Test.Sandwich.Contexts.Util.Nix (
  withWritableBinaryCache
  ) where

import Control.Monad.IO.Unlift
import Control.Monad.Logger
import Data.String.Interpolate
import Relude
import System.FilePath
import Test.Sandwich.Logging
import UnliftIO.Directory
import UnliftIO.Process
import UnliftIO.Temporary


withWritableBinaryCache :: (MonadUnliftIO m, MonadLogger m) => Maybe FilePath -> (Maybe FilePath -> m a) -> m a
withWritableBinaryCache Nothing action = action Nothing
withWritableBinaryCache (Just readOnlyPath) action =
  withSystemTempDirectory "writable-binary-cache" $ \dir -> do
    let path = dir </> "cache"
    info [i|Putting writable binary cache at: #{path}|]
    _ <- readCreateProcess (proc "cp" ["-ra", readOnlyPath, path]) ""

    -- The cache needs a writable realisations folder
    _ <- readCreateProcess (proc "chmod" ["a+w", path]) ""
    createDirectoryIfMissing True (path </> "realisations")

    action $ Just path
