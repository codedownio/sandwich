
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
  it "tries shouldBe" (2 `shouldBe` 3)
  it "tries shouldBe with Foo" (Foo 2 "asdf" (Bar 2 "asdf") `shouldBe` Foo 3 "fdsa" (Bar 3 "fdsa"))
  it "tries shouldBe with Baz" (Baz 2 "asdf" (Bar 2 "asdf") `shouldBe` Baz 3 "fdsa" (Bar 3 "fdsa"))
  it "tries shouldBe with list" ([1, 2, 3] `shouldBe` [4, 5, 6])
  it "tries shouldBe with tuple" ((1, 2, 3) `shouldBe` (4, 5, 6))
  it "tries shouldBe with list of constructors" ([Simple 1, Simple 2] `shouldBe` [Simple 3, Simple 4])
  it "tries shouldNotBe" (2 `shouldNotBe` 2)
  it "is pending" $ pending
  it "is pending with message" $ pendingWith "Not implemented yet..."
  it "does some logging" $ do
    debug "debug message"
    info "info message"
    warn "warn message"
    logError "error message"

main :: IO ()
main = runSandwich options defaultPrintFormatter verySimple
  where
    options = defaultOptions {
      optionsTestArtifactsDirectory = TestArtifactsGeneratedDirectory "test_runs" (show <$> getCurrentTime)
      }
