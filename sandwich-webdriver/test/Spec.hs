
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


-- We use nixpkgsRelease2405 here, because Chrome/chromedriver > 131 seems to have a bug where we get
-- a message like this in CI:
--
-- session not created: probably user data directory is already in use, please specify a unique value for --user-data-dir argument, or don't use --user-data-dir

spec :: TopSpecWithOptions
spec = introduceNixContext (nixpkgsRelease2505 { nixpkgsDerivationAllowUnfree = True }) $
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
