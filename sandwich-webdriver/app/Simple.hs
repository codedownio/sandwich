
module Simple where

import Test.Sandwich
import Test.Sandwich.WebDriver
import Test.WebDriver

wdOptions = (defaultWdOptions "/tmp/tools") {
  capabilities = firefoxCapabilities
  , runMode = RunHeadless defaultHeadlessConfig
  }

spec :: TopSpec
spec = introduceWebDriver wdOptions $ do
  it "opens Google and searches" $ withSession1 $ do
    openPage "http://www.google.com"
    search <- findElem (ByCSS "input[title='Search']")
    click search
    sendKeys "asdf\n" search

main :: IO ()
main = runSandwich defaultOptions spec
