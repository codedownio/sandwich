-- |

module Test.Sandwich.Formatters.Print.Types where


data PrintFormatter = PrintFormatter {
  printFormatterUseColor :: Bool
  , printFormatterIncludeLogs :: Bool
  , printFormatterIncludeCallStacks :: Bool
  , printFormatterIndentSize :: Int
  }

defaultPrintFormatter :: PrintFormatter
defaultPrintFormatter = PrintFormatter {
  printFormatterUseColor = True
  , printFormatterIncludeLogs = True
  , printFormatterIncludeCallStacks = True
  , printFormatterIndentSize = 4
  }
