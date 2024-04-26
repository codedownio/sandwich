((haskell-mode
  . (
     (haskell-process-args-stack-ghci . ("--ghci-options=-ferror-spans" "--no-build" "--no-load"
                                         "--stack-yaml" "/home/tom/tools/sandwich/stack.yaml"
                                         "sandwich-contexts-kubernetes:lib"
                                         ))
     )))
