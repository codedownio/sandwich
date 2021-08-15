
import Test.Sandwich
import Data.Time.Clock
import Test.Sandwich.Formatters.Print


verySimple :: TopSpec
verySimple = do
  it "succeeds" (return ())

main :: IO ()
main = runSandwich options verySimple
  where
    options = defaultOptions {
      optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
      }
