
module Sandwich.Contexts.Kubernetes.Util.UUID where

import qualified Data.List as L
import Data.Text as T
import Relude
import qualified System.Random as R


-- Note: for a UUID to appear in a Kubernetes name, it needs to match this regex
-- [a-z0-9]([-a-z0-9]*[a-z0-9])?(\.[a-z0-9]([-a-z0-9]*[a-z0-9])?)*'
uuidLetters :: [Char]
uuidLetters = ['a'..'z'] ++ ['0'..'9']

numUUIDLetters :: Int
numUUIDLetters = L.length uuidLetters

makeUUID :: MonadIO m => m T.Text
makeUUID = makeUUID' 8

makeUUID' :: MonadIO m => Int -> m T.Text
makeUUID' n = toText <$> (replicateM n ((uuidLetters L.!!) <$> R.randomRIO (0, numUUIDLetters - 1)))
