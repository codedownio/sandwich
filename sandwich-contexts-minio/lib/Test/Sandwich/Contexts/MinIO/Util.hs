{-# LANGUAGE CPP #-}


module Test.Sandwich.Contexts.MinIO.Util where

import Data.Char
import qualified Data.List as L
import Data.Text as T
import Relude
import qualified System.Random as R

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key             as A
import qualified Data.Aeson.KeyMap          as HM
#else
import Data.Hashable
import qualified Data.HashMap.Strict        as HM
#endif


#if MIN_VERSION_aeson(2,0,0)
aesonLookup :: Text -> HM.KeyMap v -> Maybe v
aesonLookup = HM.lookup . A.fromText
#else
aesonLookup :: (Eq k, Hashable k) => k -> HM.HashMap k v -> Maybe v
aesonLookup = HM.lookup
#endif

makeUUID :: MonadIO m => m T.Text
makeUUID = makeUUID' 8

makeUUID' :: MonadIO m => Int -> m T.Text
makeUUID' n = toText <$> (replicateM n ((uuidLetters L.!!) <$> R.randomRIO (0, numUUIDLetters - 1)))
  where
    -- Note: for a UUID to appear in a Kubernetes name, it needs to match this regex
    -- [a-z0-9]([-a-z0-9]*[a-z0-9])?(\.[a-z0-9]([-a-z0-9]*[a-z0-9])?)*'
    uuidLetters :: [Char]
    uuidLetters = ['a'..'z'] ++ ['0'..'9']

    numUUIDLetters :: Int
    numUUIDLetters = L.length uuidLetters
