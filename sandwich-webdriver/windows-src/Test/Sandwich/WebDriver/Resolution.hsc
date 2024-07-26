
module Test.Sandwich.WebDriver.Resolution (
  getResolution
  , getResolutionForDisplay
  ) where

-- TODO: implement
-- https://blog.bytellect.com/software-development/windows-programming/getting-the-windows-screen-resolution-in-c/
-- https://hackage.haskell.org/package/Win32-2.12.0.1/docs/Graphics-Win32-Window.html


getResolution :: IO (Int, Int, Int, Int)
getResolution = undefined

getResolutionForDisplay :: Int -> IO (Int, Int, Int, Int)
getResolutionForDisplay _n = undefined
