{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}

module Test.Sandwich.Contexts.Kubernetes.KindCluster.ServiceForwardIngress (
  withForwardKubernetesService'
  ) where

import Control.Monad
import Control.Monad.IO.Unlift
import Control.Monad.Logger
import qualified Data.List as L
import Data.String.Interpolate
import Data.Text as T
import qualified Data.Text.IO as T
import Network.Socket (PortNumber)
import Network.URI
import Relude hiding (withFile)
import Safe
import System.Exit
import System.FilePath
import qualified System.Random as R
import Test.Sandwich
import Test.Sandwich.Contexts.Kubernetes.Types
import Test.Sandwich.Util.Process
import Text.Regex.TDFA
import UnliftIO.Environment
import UnliftIO.Exception
import UnliftIO.IO (withFile)
import UnliftIO.Process
import UnliftIO.Temporary
import UnliftIO.Timeout


withForwardKubernetesService' :: (
  MonadUnliftIO m, MonadLoggerIO m
  , MonadReader context m
  ) => KubernetesClusterContext -> FilePath -> Text -> Text -> (URI -> m a) -> m a
withForwardKubernetesService' (KubernetesClusterContext {kubernetesClusterType=(KubernetesClusterKind {..}), ..}) kubectlBinary namespace service action = do
  baseEnv <- maybe getEnvironment return kindClusterEnvironment
  let env = L.nubBy (\x y -> fst x == fst y) (("KUBECONFIG", kubernetesClusterKubeConfigPath) : baseEnv)

  randomHost <- generateRandomHostname

  withSystemTempDirectory "ingress.yaml" $ \dir -> do
    let configFile = dir </> "ingress.yaml"
    liftIO $ T.writeFile configFile (ingressConfig service randomHost)

    createProcessWithLogging ((proc kubectlBinary ["create"
                                                  , "--namespace", toString namespace
                                                  , "-f", configFile]) {
                                 env = Just env
                                 }) >>= waitForProcess >>= (`shouldBe` ExitSuccess)

  -- TODO: wait for ingress to be ready?
  -- Possibly not necessary since the server context waits for 200 after this

  nodes <- ((T.words . toText) <$> (readCreateProcessWithLogging ((shell [i|kind get nodes --name "#{kubernetesClusterName}"|]) { env = Just env }) ""))
  controlPlaneNode <- case headMay [t | t <- nodes, "control-plane" `T.isInfixOf` t] of
    Nothing -> expectationFailure [i|Couldn't find control plane node (had: #{nodes})|]
    Just x -> pure x

  hostAndPort <- (T.strip . toText) <$> readCreateProcessWithLogging (
    proc (toString kindClusterDriver) [
      "port", toString controlPlaneNode, "80/tcp"
      ]) ""
  let caddyArgs :: [String] = [
        "reverse-proxy"
        , "--from", "http://localhost:0"
        , "--to", "http://" <> toString hostAndPort
        , "--header-up", [i|Host: #{randomHost}|]
        ]

  info [i|caddy args: #{T.intercalate " " $ fmap toText caddyArgs}|]
  (stdoutRead, stdoutWrite) <- createPipe
  withFile "/dev/null" WriteMode $ \hNull -> do
    let cp = (proc "caddy" caddyArgs) {
          std_err = UseHandle stdoutWrite
          , std_out = UseHandle hNull
          , create_group = True
          }
    bracket (createProcess cp) (\(_, _, _, p) -> gracefullyStopProcess p 60_000_000) $ \_ -> do
      uriToUseMaybe <- timeout 60_000_000 $ fix $ \loop -> do
        line <- liftIO $ T.hGetLine stdoutRead
        debug [i|caddy: #{line}|]
        maybe loop pure (parsePort line)

      uriToUse <- case uriToUseMaybe of
        Nothing -> expectationFailure [i|Timed out waiting for caddy line with actual port|]
        Just x -> pure [i|http://localhost:#{x}|]
      info [i|uriToUse: #{uriToUse}|]

      action =<< case parseURI uriToUse of
        Nothing -> expectationFailure [i|Couldn't parse URI in withForwardKubernetesService': #{uriToUse}|]
        Just x -> pure x

withForwardKubernetesService' _ _ _ _ _ = error "withForwardKubernetesService' must be called with a kind KubernetesClusterContext"


ingressConfig :: Text -> Text -> Text
ingressConfig service host = [i|
apiVersion: networking.k8s.io/v1
kind: Ingress
metadata:
  name: #{service}-ingress
spec:
  rules:
  - host: #{host}
    http:
      paths:
      - path: /
        pathType: Prefix
        backend:
          service:
            name: #{service}
            port:
              number: 80
|]


generateRandomHostname :: MonadIO m => m Text
generateRandomHostname = (toText <$>) $ liftIO $ do
  labelCount <- R.randomRIO (1, 5)
  labels <- replicateM labelCount (randomAlpha =<< R.randomRIO (5, 10))
  let hostname = L.intercalate "." labels
  return hostname
  where
    randomAlpha :: Int -> IO String
    randomAlpha len = do
      gen <- R.newStdGen
      return $ L.take len (R.randomRs ('a', 'z') gen)


-- test :: Text
-- test = [i|"actual_address": "[::]:46763"|]

parsePort :: Text -> Maybe PortNumber
parsePort t = case t =~~ ([i|"actual_address":[[:space:]]*"\\[::\\]:([[:digit:]]+)"|] :: Text) of
  Just ((_before, _fullMatch, _after, [(readMay . toString) -> Just p]) :: (Text, Text, Text, [Text])) -> Just p
  _ -> Nothing
