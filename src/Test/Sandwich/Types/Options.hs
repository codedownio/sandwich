-- |

module Test.Sandwich.Types.Options where


data Options = Options {
  optionsMaxParallelJobs :: Int
  }

defaultOptions = Options {
  optionsMaxParallelJobs = 1
  }
