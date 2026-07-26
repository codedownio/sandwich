
module Test.Sandwich.Formatters.TerminalUI.SpeedScope (
  SpeedScopeServer(..)
  , openSpeedScope
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Logger
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as L
import Data.String.Interpolate
import qualified Data.Text as T
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Exit
import System.FilePath
import System.Process
import Test.Sandwich.Formatters.TerminalUI.CrossPlatform
import Test.Sandwich.ManagedAsync
import Test.Sandwich.TestTimer
import Test.Sandwich.Types.RunTree
import Test.Sandwich.Types.TestTimer
import UnliftIO.Async
import UnliftIO.Directory
import UnliftIO.Exception
import UnliftIO.MVar
import UnliftIO.Temporary


speedScopeVersion :: String
speedScopeVersion = "1.25.0"

speedScopeUrl :: String
speedScopeUrl = [i|https://registry.npmjs.org/speedscope/-/speedscope-#{speedScopeVersion}.tgz|]

profilePath :: String
profilePath = "/speedscope.json"

data SpeedScopeServer = SpeedScopeServer {
  speedScopeServerSocket :: Socket
  , speedScopeServerAsync :: Async ()
  , speedScopeServerUrl :: String
  }

openSpeedScope :: (MonadLoggerIO m, MonadUnliftIO m) => MVar (Maybe SpeedScopeServer) -> BaseContext -> m ()
openSpeedScope serverVar (BaseContext {..}) = handle logException $
  modifyMVar_ serverVar $ \maybeServer -> do
    server <- case maybeServer of
      Just server -> return server
      Nothing -> do
        bundleDir <- ensureSpeedScopeBundle
        server <- liftIO $ startServer baseContextRunId baseContextTestTimer bundleDir
        logDebugN [i|Serving speedscope at #{speedScopeServerUrl server}|]
        return server

    liftIO $ openUrlPortable (speedScopeServerUrl server)
    return $ Just server
  where
    logException (e :: SomeException) = logDebugN [i|Failed to open speedscope: #{e}|]

-- * Bundle

ensureSpeedScopeBundle :: (MonadLoggerIO m, MonadUnliftIO m) => m (Maybe FilePath)
ensureSpeedScopeBundle = flip catch logException $ do
  cacheDir <- getXdgDirectory XdgCache ("sandwich" </> ("speedscope-" <> speedScopeVersion))
  doesFileExist (cacheDir </> "index.html") >>= \case
    True -> return $ Just cacheDir
    False -> withSystemTempDirectory "sandwich-speedscope" $ \tmpDir -> do
      let tarball = tmpDir </> "speedscope.tgz"
      logDebugN [i|Downloading #{speedScopeUrl}|]
      run "curl" ["-sSL", speedScopeUrl, "-o", tarball]
      run "tar" ["xzf", tarball, "-C", tmpDir]

      -- The npm tarball puts the self-contained app in package/dist/release.
      let releaseDir = tmpDir </> "package" </> "dist" </> "release"
      doesFileExist (releaseDir </> "index.html") >>= \case
        False -> do
          logDebugN [i|No index.html in the speedscope tarball; falling back to speedscope.app|]
          return Nothing
        True -> do
          createDirectoryIfMissing True cacheDir
          files <- listDirectory releaseDir
          forM_ files $ \file -> copyFile (releaseDir </> file) (cacheDir </> file)
          logDebugN [i|Unpacked speedscope to #{cacheDir}|]
          return $ Just cacheDir
  where
    logException (e :: SomeException) = do
      logDebugN [i|Couldn't fetch speedscope (#{e}); falling back to speedscope.app|]
      return Nothing

    run cmd args = liftIO (readProcessWithExitCode cmd args "") >>= \case
      (ExitSuccess, _, _) -> return ()
      (code, _, stderr') -> throwIO $ userError [i|#{cmd} failed (#{code}): #{stderr'}|]

-- * Server

startServer :: T.Text -> TestTimer -> Maybe FilePath -> IO SpeedScopeServer
startServer runId testTimer bundleDir = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen sock 16
  port <- socketPort sock

  asy <- managedAsync runId "speedscope-server" $ void $ forever $ do
    (conn, _) <- accept sock
    void $ managedAsync runId "speedscope-request" $
      handleRequest conn `finally` close conn

  let url = case bundleDir of
        Just _ -> [i|http://127.0.0.1:#{port}/index.html\#profileURL=#{profilePath}&title=Sandwich|]
        -- This won't work on Safari, since it treats it as mixed content.
        Nothing -> [i|https://www.speedscope.app/\#profileURL=http%3A%2F%2F127.0.0.1%3A#{port}#{profilePath}&title=Sandwich|]

  return $ SpeedScopeServer {
    speedScopeServerSocket = sock
    , speedScopeServerAsync = asy
    , speedScopeServerUrl = url
    }

  where
    handleRequest conn = handle (\(_ :: SomeException) -> return ()) $ do
      path <- requestPath conn
      case path of
        Nothing -> respond conn "400 Bad Request" "text/plain" "Bad request"
        Just p | p == profilePath -> renderSpeedScopeFile testTimer >>= \case
          Nothing -> respond conn "404 Not Found" "text/plain" "No test timer is running"
          Just contents -> respond conn "200 OK" "application/json" contents
        Just p -> case bundleDir of
          Nothing -> respond conn "404 Not Found" "text/plain" "Not found"
          Just dir -> case bundleFile dir p of
            Nothing -> respond conn "404 Not Found" "text/plain" "Not found"
            Just file -> doesFileExist file >>= \case
              False -> respond conn "404 Not Found" "text/plain" "Not found"
              True -> do
                contents <- BL.readFile file
                respond conn "200 OK" (contentType file) contents

    bundleFile :: FilePath -> String -> Maybe FilePath
    bundleFile dir path = case segments of
      [] -> Just (dir </> "index.html")
      [file] | isValid file, not (isAbsolute file), file /= ".." -> Just (dir </> file)
      _ -> Nothing
      where
        segments = filter (`notElem` ["", ".", "/"]) $ splitDirectories $
          takeWhile (`notElem` ("?#" :: String)) path

    requestPath :: Socket -> IO (Maybe String)
    requestPath conn = go mempty
      where
        go acc
          | BS.length acc > 16384 = return Nothing
          | otherwise = case BS8.lines acc of
              (firstLine:_:_) -> return $ case BS8.words firstLine of
                (_method:path:_) -> Just (BS8.unpack path)
                _ -> Nothing
              _ -> recv conn 4096 >>= \chunk -> if BS.null chunk then return Nothing else go (acc <> chunk)

    respond :: Socket -> String -> String -> BL.ByteString -> IO ()
    respond conn status contentType' body = do
      sendAll conn $ BS8.pack $ L.intercalate "\r\n" [
        [i|HTTP/1.1 #{status}|]
        , [i|Content-Type: #{contentType'}|]
        , [i|Content-Length: #{BL.length body}|]
        -- So that speedscope.app can fetch the profile from us, when we're falling back to it.
        , "Access-Control-Allow-Origin: *"
        , "Cache-Control: no-store"
        , "Connection: close"
        , "", ""
        ]
      mapM_ (sendAll conn) (BL.toChunks body)

    contentType :: FilePath -> String
    contentType file = case takeExtension file of
      ".html" -> "text/html; charset=utf-8"
      ".js" -> "text/javascript"
      ".css" -> "text/css"
      ".json" -> "application/json"
      ".wasm" -> "application/wasm"
      ".woff2" -> "font/woff2"
      ".png" -> "image/png"
      ".ico" -> "image/x-icon"
      ".txt" -> "text/plain"
      ".md" -> "text/plain"
      _ -> "application/octet-stream"
