{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}

module Test.Sandwich.Types.TestTimer where

import Control.Concurrent
import Data.Aeson as A
import Data.Aeson.TH as A
import qualified Data.List as L
import Data.Sequence
import qualified Data.Text as T
import Data.Time.Clock.POSIX
import Lens.Micro.TH
import System.IO
import Test.Sandwich.Types.Spec
import Test.Sandwich.Types.TestTimer.LensRules (testTimerLensRules)


-- * SpeedScope types

data SpeedScopeFrame = SpeedScopeFrame {
  _name :: T.Text
  } deriving (Show, Eq)
$(deriveJSON (A.defaultOptions {
                 A.fieldLabelModifier = L.drop 1
                 , A.sumEncoding = A.UntaggedValue
                 }) ''SpeedScopeFrame)
$(makeLensesWith testTimerLensRules ''SpeedScopeFrame)

data SpeedScopeShared = SpeedScopeShared {
  _frames :: Seq SpeedScopeFrame
  } deriving Show
$(deriveJSON (A.defaultOptions {
                 A.fieldLabelModifier = L.drop 1
                 , A.sumEncoding = A.UntaggedValue
                 }) ''SpeedScopeShared)
$(makeLensesWith testTimerLensRules ''SpeedScopeShared)

data SpeedScopeEventType = SpeedScopeEventTypeOpen | SpeedScopeEventTypeClose
  deriving (Show, Eq)
$(deriveJSON (A.defaultOptions {
                 A.constructorTagModifier = L.take 1 . L.drop (L.length ("SpeedScopeEventType" :: String))
                 , A.sumEncoding = A.UntaggedValue
                 }) ''SpeedScopeEventType)

data SpeedScopeEvent = SpeedScopeEvent {
  _typ :: SpeedScopeEventType
  , _frame :: Int
  , _at :: POSIXTime
  } deriving Show
$(deriveJSON (A.defaultOptions {
                 A.fieldLabelModifier = \x -> case x of
                     "_typ" -> "type"
                     _ -> L.drop 1 x
                 , A.sumEncoding = A.UntaggedValue
                 }) ''SpeedScopeEvent)
$(makeLensesWith testTimerLensRules ''SpeedScopeEvent)

data SpeedScopeProfile = SpeedScopeProfile {
  _typ :: T.Text
  , _name :: T.Text
  , _unit :: T.Text
  , _startValue :: POSIXTime
  , _endValue :: POSIXTime
  , _events :: Seq SpeedScopeEvent
  } deriving Show
$(deriveJSON (A.defaultOptions {
                 A.fieldLabelModifier = \x -> case x of
                     "_typ" -> "type"
                     _ -> L.drop 1 x
                 , A.sumEncoding = A.UntaggedValue
                 }) ''SpeedScopeProfile)
$(makeLensesWith testTimerLensRules ''SpeedScopeProfile)

data SpeedScopeFile = SpeedScopeFile {
  _exporter :: T.Text
  , _name :: T.Text
  , _activeProfileIndex :: Int
  , _schema :: T.Text
  , _shared :: SpeedScopeShared
  , _profiles :: [SpeedScopeProfile]
  } deriving Show
$(deriveJSON (A.defaultOptions {
                 A.fieldLabelModifier = \x -> case x of
                     "_schema" -> "$schema"
                     _ -> L.drop 1 x
                 , A.sumEncoding = A.UntaggedValue
                 }) ''SpeedScopeFile)
$(makeLensesWith testTimerLensRules ''SpeedScopeFile)

emptySpeedScopeFile :: SpeedScopeFile
emptySpeedScopeFile =
  SpeedScopeFile {
    _exporter = "sandwich-test-exporter"
    , _name = "sandwich-test"
    , _activeProfileIndex = 0
    , _schema = "https://www.speedscope.app/file-format-schema.json"
    , _shared = SpeedScopeShared {
        _frames = mempty
        }
    , _profiles = []
    }

newProfile :: T.Text -> POSIXTime -> SpeedScopeProfile
newProfile profileName startTime = SpeedScopeProfile {
  _typ = "evented"
  , _name = profileName
  , _unit = "seconds"
  , _startValue = startTime
  , _endValue = startTime
  , _events = mempty
  }

-- * Main type

data TestTimer = SpeedScopeTestTimer {
  testTimerBasePath :: FilePath
  , testTimerHandle :: Maybe Handle
  , testTimerSpeedScopeFile :: MVar SpeedScopeFile
  } | NullTestTimer

-- * Labels and classes

defaultProfileName :: T.Text
defaultProfileName = "default"

class HasTestTimer context where
  getTestTimer :: context -> TestTimer

testTimerProfile :: Label "testTimerProfile" TestTimerProfile
testTimerProfile = Label :: Label "testTimerProfile" TestTimerProfile

newtype TestTimerProfile = TestTimerProfile T.Text
