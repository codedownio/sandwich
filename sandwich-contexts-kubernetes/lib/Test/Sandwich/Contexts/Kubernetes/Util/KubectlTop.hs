{-|

Helpers for @kubectl top pods@ output and namespace selection, shared by the
resource watcher ("Test.Sandwich.Contexts.Kubernetes.ResourceWatcher") and the OOM
watcher ("Test.Sandwich.Contexts.Kubernetes.OOMWatcher").

-}

module Test.Sandwich.Contexts.Kubernetes.Util.KubectlTop (
  namespaceArgs
  , scopeLabel
  , parseTop
  , parseCpu
  , parseMem
  ) where

import Data.String.Interpolate
import qualified Data.Text as T
import Relude


-- | @kubectl@ args selecting a namespace, or all namespaces when 'Nothing'.
namespaceArgs :: Maybe Text -> [String]
namespaceArgs = maybe ["--all-namespaces"] (\ns -> ["-n", toString ns])

-- | Human-readable description of a watched namespace scope, for log lines and
-- failure messages.
scopeLabel :: Maybe Text -> Text
scopeLabel = maybe "all namespaces" (\ns -> [i|namespace '#{ns}'|])

-- | Parse @kubectl top pods --no-headers@ output into @(key, cpuMillicores,
-- memoryBytes)@ triples. The last two columns are always CPU and memory; every
-- leading column is part of the identifier, joined with @\/@. This handles all
-- shapes without extra flags:
--
--   * @pod@                              (default)
--   * @pod\/container@                   (@--containers@)
--   * @namespace\/pod@                   (@--all-namespaces@)
--   * @namespace\/pod\/container@        (@--all-namespaces --containers@)
--
-- Rows whose CPU or memory column doesn't parse are dropped. @kubectl top@ prints
-- @\<unknown\>@ for both while metrics-server is still warming up (and sometimes for
-- an individual pod), and we'd rather skip those than record bogus zero samples.
parseTop :: Text -> [(Text, Integer, Integer)]
parseTop = mapMaybe parseLine . lines
  where
    parseLine l = case reverse (words l) of
      (mem : cpu : ident@(_ : _)) ->
        (T.intercalate "/" (reverse ident),,) <$> parseCpu cpu <*> parseMem mem
      _ -> Nothing

-- | Parse a kubectl CPU quantity (e.g. @12m@, @1@) into millicores, or 'Nothing'
-- if it doesn't look like a CPU quantity (e.g. @\<unknown\>@).
parseCpu :: Text -> Maybe Integer
parseCpu t = case T.stripSuffix "m" t of
  Just n -> readMaybe (toString n)
  Nothing -> (\d -> round (d * 1000)) <$> (readMaybe (toString t) :: Maybe Double)

-- | Parse a kubectl memory quantity (e.g. @345Mi@, @1Gi@, @512Ki@) into bytes, or
-- 'Nothing' if it doesn't look like a memory quantity (e.g. @\<unknown\>@).
parseMem :: Text -> Maybe Integer
parseMem t = asum [
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
