{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Data.String.Interpolate
import Options.Applicative
import Test.Sandwich

data MyArgs = MyArgs {
  myArgsFoo :: String
  , myArgsBar :: Int
  }

myArgsParser :: Parser MyArgs
myArgsParser = MyArgs
  <$> strOption (long "foo" <> help "Value of foo" <> metavar "STRING")
  <*> option auto (long "bar" <> showDefault <> help "Value of bar" <> value 1 <> metavar "INT")

commandLineArgsDemo :: TopSpecWithOptions' MyArgs
commandLineArgsDemo = describe "Custom command line args" $ do
  it "Uses the custom command line args" $ do
    MyArgs {..} <- getUserCommandLineOptions
    warn [i|Got foo: #{myArgsFoo}|]
    warn [i|Got bar: #{myArgsBar}|]

main :: IO ()
main = runSandwichWithCommandLineArgs' testOptions myArgsParser commandLineArgsDemo

testOptions = defaultOptions {
  optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
  }
