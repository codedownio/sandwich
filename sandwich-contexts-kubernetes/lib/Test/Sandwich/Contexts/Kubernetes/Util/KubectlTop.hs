{-|

Helpers for @kubectl top pods@ output and namespace selection, shared by the
resource watcher ("Test.Sandwich.Contexts.Kubernetes.ResourceWatcher") and the OOM
watcher ("Test.Sandwich.Contexts.Kubernetes.OOMWatcher").

-}

module Test.Sandwich.Contexts.Kubernetes.Util.KubectlTop (
  namespaceArgs
  , parseTop
  , parseCpu
  , parseMem
  ) where

import qualified Data.Text as T
import Relude


-- | @kubectl@ args selecting a namespace, or all namespaces when 'Nothing'.
namespaceArgs :: Maybe Text -> [String]
namespaceArgs = maybe ["--all-namespaces"] (\ns -> ["-n", toString ns])

-- | Parse @kubectl top pods --no-headers@ output into @(key, cpuMillicores,
-- memoryBytes)@ triples. The last two columns are always CPU and memory; every
-- leading column is part of the identifier, joined with @\/@. This handles all
-- shapes without extra flags:
--
--   * @pod@                              (default)
--   * @pod\/container@                   (@--containers@)
--   * @namespace\/pod@                   (@--all-namespaces@)
--   * @namespace\/pod\/container@        (@--all-namespaces --containers@)
parseTop :: Text -> [(Text, Integer, Integer)]
parseTop = mapMaybe parseLine . lines
  where
    parseLine l = case reverse (words l) of
      (mem : cpu : ident@(_ : _)) ->
        Just (T.intercalate "/" (reverse ident), parseCpu cpu, parseMem mem)
      _ -> Nothing

-- | Parse a kubectl CPU quantity (e.g. @12m@, @1@) into millicores.
parseCpu :: Text -> Integer
parseCpu t = case T.stripSuffix "m" t of
  Just n -> fromMaybe 0 (readMaybe (toString n))
  Nothing -> maybe 0 (\d -> round (d * 1000)) (readMaybe (toString t) :: Maybe Double)

-- | Parse a kubectl memory quantity (e.g. @345Mi@, @1Gi@, @512Ki@) into bytes.
parseMem :: Text -> Integer
parseMem t = fromMaybe 0 $ asum [
  parseWith "Ki" 1024
  , parseWith "Mi" (1024 * 1024)
  , parseWith "Gi" (1024 * 1024 * 1024)
  , parseWith "Ti" (1024 * 1024 * 1024 * 1024)
  , parseWith "k" 1000
  , parseWith "M" 1000000
  , parseWith "G" 1000000000
  , readMaybe (toString t)  -- plain bytes
  ]
  where
    parseWith suf mult = do
      n <- T.stripSuffix suf t
      d <- readMaybe (toString n) :: Maybe Double
      Just (round (d * mult))
