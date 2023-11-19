
import Test.Sandwich
import Test.Sandwich.Formatters.Print


verySimple :: TopSpec
verySimple = do
  it "succeeds" (return ())

main :: IO ()
main = runSandwich options verySimple
  where
    options = defaultOptions {
      optionsTestArtifactsDirectory = defaultTestArtifactsDirectory
      }
