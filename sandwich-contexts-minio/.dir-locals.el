((haskell-mode
  . (
     (haskell-process-args-stack-ghci . ("--ghci-options=-ferror-spans" "--no-build" "--no-load"
                                         "sandwich-contexts-minio:lib"
                                         ))
     )))
