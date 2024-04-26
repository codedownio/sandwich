
module Sandwich.Contexts.Kubernetes.KindCluster.Network () where

-- linkContainer :: (MonadLoggerIO m, MonadCatch m) => Text -> m ()
-- linkContainer containerName = do
--   -- Connect the container to the "kind" network
--   ds <- getDockerState False
--   isConnectedToNetwork ds (Id containerName) "kind" >>= \case
--     Left err -> throw $ CodeDownException [i|Error checking if container '#{containerName}' is connected to network: '#{err}'|] (Just callStack)
--     Right True -> info [i|Container '#{containerName}' was already connected to "kind" network|]
--     Right False -> do
--       info [i|Connecting container '#{containerName}' to "kind" network|]
--       connectNetwork ds containerName "kind" >>= \case
--         Left err -> logError [i|Failed to link container #{containerName}: #{err}|]
--         Right () -> return ()
