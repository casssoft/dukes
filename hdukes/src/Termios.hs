{-# LANGUAGE ForeignFunctionInterface #-}
module Termios where
import Foreign.C
 
foreign import ccall "reset_terminal_mode" reset_terminal_mode :: CInt -> IO ()
foreign import ccall "set_conio_terminal_mode" set_conio_terminal_mode :: CInt -> IO ()
foreign import ccall "loadSprites" loadSprites :: CInt -> IO ()
foreign import ccall "drawScene" c_drawScene :: CInt -> CString -> IO ()

drawScene :: CInt -> String -> IO ()
drawScene x text = withCString text (c_drawScene x)
