((haskell-mode
  . (
     (haskell-process-args-stack-ghci . ("--ghci-options=-ferror-spans" "--no-build" "--no-load"
                                         "sandwich-webdriver:lib"
                                         "sandwich-webdriver:test:sandwich-webdriver-test"
                                         ))
     )))
