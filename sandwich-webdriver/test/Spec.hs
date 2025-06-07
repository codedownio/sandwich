
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
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy as BL


-- We use nixpkgsRelease2405 here, because Chrome/chromedriver > 131 seems to have a bug where we get
-- a message like this in CI:
--
-- session not created: probably user data directory is already in use, please specify a unique value for --user-data-dir argument, or don't use --user-data-dir

spec :: TopSpecWithOptions
spec = introduceNixContext (nixpkgsRelease2405 { nixpkgsDerivationAllowUnfree = True }) $
  introduceWebDriverViaNix defaultWdOptions $ do
    it "opens Google and searches" $ withSession1 $ do
      openPage [i|https://www.google.com|]
      search <- findElem (ByCSS [i|*[title="Search"]|])
      click search
      sendKeys "Haskell Sandwich" search
      findElem (ByCSS [i|input[type="submit"]|]) >>= click

      Just dir <- getCurrentFolder
      screenshot >>= liftIO . BL.writeFile (dir </> "screenshot.png")


main :: IO ()
main = runSandwichWithCommandLineArgs options spec
  where
    options = defaultOptions {
      optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
      }
