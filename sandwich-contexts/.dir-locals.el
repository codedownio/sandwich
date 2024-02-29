((haskell-mode
  . (
     (haskell-process-args-stack-ghci . ("--ghci-options=-ferror-spans" "--no-build" "--no-load"
                                         "--stack-yaml" "/home/tom/codedown/stack.yaml"
                                         "codedown-core:lib"
                                         "codedown-test-contexts:lib"
                                         ))
     )))
