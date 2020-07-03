-- |

module Test.Sandwich.Formatters.Print.Types where


data PrintFormatter = PrintFormatter {
  printFormatterUseColor :: Bool
  , printFormatterIncludeLogs :: Bool
  , printFormatterIndentSize :: Int
  }

defaultPrintFormatter :: PrintFormatter
defaultPrintFormatter = PrintFormatter {
  printFormatterUseColor = True
  , printFormatterIncludeLogs = True
  , printFormatterIndentSize = 4
  }
