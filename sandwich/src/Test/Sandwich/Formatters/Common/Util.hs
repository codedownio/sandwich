{-# LANGUAGE CPP #-}

module Test.Sandwich.Formatters.Common.Util (
  formatNominalDiffTime
  ) where

import Data.Fixed
import Data.Time.Clock
import Text.Printf

formatNominalDiffTime :: NominalDiffTime -> String
formatNominalDiffTime diff | diff < ps = (roundFixed ((nominalDiffTimeToSeconds diff) * 10^(15 :: Integer))) <> "ps"
formatNominalDiffTime diff | diff < ns = (roundFixed ((nominalDiffTimeToSeconds diff) * 10^(12 :: Integer))) <> "ns"
formatNominalDiffTime diff | diff < us = (roundFixed ((nominalDiffTimeToSeconds diff) * 10^(9 :: Integer))) <> "ns"
formatNominalDiffTime diff | diff < ms = (roundFixed ((nominalDiffTimeToSeconds diff) * 10^(6 :: Integer))) <> "us"
formatNominalDiffTime diff | diff < second = (roundFixed ((nominalDiffTimeToSeconds diff) * 10^(3 :: Integer))) <> "ms"
formatNominalDiffTime diff = (roundFixed (nominalDiffTimeToSeconds diff)) <> "s"

second, ms, us, ns, ps :: NominalDiffTime
second = secondsToNominalDiffTime 1
ms = secondsToNominalDiffTime 0.001
us = secondsToNominalDiffTime 0.000001
ns = secondsToNominalDiffTime 0.000000001
ps = secondsToNominalDiffTime 0.000000000001

roundFixed :: Fixed E12 -> String
roundFixed f = printf "%.1f" ((realToFrac f) :: Double)


#if !MIN_VERSION_time(1,9,1)
secondsToNominalDiffTime :: Pico -> NominalDiffTime
secondsToNominalDiffTime = realToFrac

nominalDiffTimeToSeconds :: NominalDiffTime -> Pico
nominalDiffTimeToSeconds = realToFrac
#endif
