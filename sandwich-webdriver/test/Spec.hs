
import Test.Sandwich
import Test.Sandwich.WebDriver.Config
import Data.Text as T
import UnliftIO.Temporary
import Data.Time.Clock
import Data.String.Interpolate


spec :: TopSpec
spec = do
  it "successfully runs obtainChromeDriver" $ do
    withSystemTempDirectory "test-download" $ \dir -> do
      obtainChromeDriver dir (DownloadChromeDriverAutodetect Nothing) >>= \case
        Right x -> info [i|Got chromedriver: #{x}|]
        Left err -> expectationFailure (T.unpack err)

  it "successfully runs obtainGeckoDriver" $ do
    withSystemTempDirectory "test-download" $ \dir -> do
      obtainGeckoDriver dir (DownloadGeckoDriverAutodetect Nothing) >>= \case
        Right x -> info [i|Got geckoDriver: #{x}|]
        Left err -> expectationFailure (T.unpack err)


main :: IO ()
main = runSandwich options spec
  where
    options = defaultOptions {
      optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
      }
