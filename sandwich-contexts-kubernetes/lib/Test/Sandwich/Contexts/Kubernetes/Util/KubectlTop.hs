{-|

Parsers for @kubectl top pods@ output, shared by the memory and CPU watchers
("Test.Sandwich.Contexts.Kubernetes.MemoryWatcher" and
"Test.Sandwich.Contexts.Kubernetes.CpuWatcher").

-}

module Test.Sandwich.Contexts.Kubernetes.Util.KubectlTop (
  parseTopOutput
  , parseContainerTopOutput
  , parseCpu
  , parseMem
  ) where

import qualified Data.Text as T
import Relude


-- | Parse the output of @kubectl top pods --no-headers@ into
-- @(pod, cpuMillicores, memoryBytes)@ triples.
parseTopOutput :: Text -> [(Text, Integer, Integer)]
parseTopOutput = mapMaybe parseLine . lines
  where
    parseLine l = case words l of
      (name : cpu : mem : _) -> Just (name, parseCpu cpu, parseMem mem)
      _ -> Nothing

-- | Parse the output of @kubectl top pods --containers --no-headers@ into
-- @(pod\/container, cpuMillicores, memoryBytes)@ triples.
parseContainerTopOutput :: Text -> [(Text, Integer, Integer)]
parseContainerTopOutput = mapMaybe parseLine . lines
  where
    parseLine l = case words l of
      (pod : container : cpu : mem : _) -> Just (pod <> "/" <> container, parseCpu cpu, parseMem mem)
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
