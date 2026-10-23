
import Test.Sandwich
import Test.Sandwich.WebDriver.Config
import Test.Sandwich.WebDriver.Binaries
import Data.Text as T
import UnliftIO.Temporary
import Data.Time.Clock
import Data.String.Interpolate
import Test.Sandwich.Contexts.Nix
import System.FilePath
import Test.WebDriver.Commands
import Test.Sandwich.WebDriver
import Test.Sandwich.Waits
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BL


-- We use nixpkgsMaster rather than a release pin because google-chrome is unfree, so it isn't in
-- the Nix binary cache and has to be downloaded from Google. Google only serves the current couple
-- of Chrome releases, so the google-chrome in an older pin fails to fetch with a 404. Bump
-- nixpkgsMaster whenever that happens.

spec :: TopSpecWithOptions
spec = introduceNixContext (nixpkgsMaster { nixpkgsDerivationAllowUnfree = True }) $
  introduceWebDriverViaNix defaultWdOptions $ do
    it "opens Xkcd and presses the prev button" $ withSession1 $ do
      openPage [i|https://www.xkcd.com|]
      origUrl <- getCurrentURL
      prev <- findElem (ByCSS [i|a[rel=prev]|])
      click prev

      waitUntil 30 $ do
        url <- getCurrentURL
        url `shouldNotBe` origUrl

      Just dir <- getCurrentFolder
      screenshot >>= liftIO . BL.writeFile (dir </> "screenshot.png")


main :: IO ()
main = runSandwichWithCommandLineArgs options spec
  where
    options = defaultOptions {
      optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
      }
