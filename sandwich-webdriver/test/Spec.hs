
import Test.Sandwich
import Data.Time.Clock
import Test.Sandwich.Formatters.Print

data Foo = Foo { fooInt :: Int, fooString :: String, fooBar :: Bar } deriving (Show, Eq)
data Bar = Bar { barInt :: Int, barString :: String } deriving (Show, Eq)
data Baz = Baz Int String Bar deriving (Show, Eq)
data Simple = Simple { simpleInt :: Int } deriving (Show, Eq)


verySimple :: TopSpec
verySimple = do
  it "succeeds" (return ())

main :: IO ()
main = runSandwich options verySimple
  where
    options = defaultOptions {
      optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
      }
