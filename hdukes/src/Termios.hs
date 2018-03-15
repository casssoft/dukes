{-# LANGUAGE ForeignFunctionInterface #-}
module Termios where
import Foreign.C
 
foreign import ccall "reset_terminal_mode" reset_terminal_mode :: CInt -> IO ()
foreign import ccall "set_conio_terminal_mode" set_conio_terminal_mode :: CInt -> IO ()
foreign import ccall "loadSprites" loadSprites :: CInt -> IO ()
foreign import ccall "drawScene" c_drawScene :: CString -> CString -> IO ()

drawScene :: String -> String -> IO ()
drawScene sprite text = withCString text (\ctext ->
    withCString sprite
    (\csprite -> c_drawScene csprite ctext))
