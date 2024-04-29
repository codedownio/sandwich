{-# LANGUAGE CPP #-}

module Test.Sandwich.Contexts.Kubernetes.Util.Aeson where

import qualified Data.Aeson as A
import Data.Char
import qualified Data.List as L
import Data.Text hiding (toLower)
import Relude

#if MIN_VERSION_aeson(2,0,0)
import qualified Data.Aeson.Key             as A
import qualified Data.Aeson.KeyMap          as HM
#else
import Data.Hashable
import qualified Data.HashMap.Strict        as HM
#endif


textKeys :: A.Object -> [Text]
#if MIN_VERSION_aeson(2,0,0)
textKeys = fmap A.toText . HM.keys
#else
textKeys = HM.keys
#endif

#if MIN_VERSION_aeson(2,0,0)
aesonLookup :: Text -> HM.KeyMap v -> Maybe v
aesonLookup = HM.lookup . A.fromText
#else
aesonLookup :: (Eq k, Hashable k) => k -> HM.HashMap k v -> Maybe v
aesonLookup = HM.lookup
#endif

#if MIN_VERSION_aeson(2,0,0)
aesonInsert :: Text -> v -> HM.KeyMap v -> HM.KeyMap v
aesonInsert t = HM.insert (A.fromText t)
#else
aesonInsert :: (Eq k, Hashable k) => k -> v -> HM.HashMap k v -> HM.HashMap k v
aesonInsert = HM.insert
#endif

#if MIN_VERSION_aeson(2,0,0)
aesonDelete :: Text -> HM.KeyMap v -> HM.KeyMap v
aesonDelete t = HM.delete (A.fromText t)
#else
aesonDelete :: (Eq k, Hashable k) => k -> HM.HashMap k v -> HM.HashMap k v
aesonDelete = HM.delete
#endif

#if MIN_VERSION_aeson(2,0,0)
aesonToList :: HM.KeyMap v -> [(A.Key, v)]
aesonToList = HM.toList
#else
aesonToList :: HM.HashMap k v -> [(k, v)]
aesonToList = HM.toList
#endif


dropNAndCamelCase :: Int -> String -> String
dropNAndCamelCase n = lowercaseFirst . L.drop n

lowercaseFirst :: [Char] -> [Char]
lowercaseFirst (x:xs) = (toLower x) : xs
lowercaseFirst [] = []
