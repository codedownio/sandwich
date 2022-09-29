{-# LANGUAGE CPP #-}

module Test.Sandwich.Signals (
  installHandler
  , sigINT
  , sigTERM
  ) where

import Foreign.C.Types

#ifdef mingw32_HOST_OS
import Control.Exception.Base (assert)
import Foreign
#else
import Control.Monad
import qualified System.Posix.Signals as Posix
#endif


type Signal = CInt

type Handler = Signal -> IO ()

sigINT :: Signal
sigINT = 2

sigTERM :: Signal
sigTERM = 15

installHandler :: Signal -> Handler -> IO ()
#ifdef mingw32_HOST_OS
foreign import ccall "wrapper"
    genHandler:: Handler -> IO (FunPtr Handler)

foreign import ccall safe "signal.h signal"
    install:: Signal -> FunPtr Handler -> IO Signal

installHandler signal handler = do
    result <- install signal =<< genHandler handler
    return $ assert (result == 0) ()
#else
installHandler signal handler = void $ Posix.installHandler signal (Posix.CatchInfo (handler . Posix.siginfoSignal)) Nothing
#endif
