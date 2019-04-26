module Main where
import Control.Exception
import System.IO
import Termios as Termios
import Duke as Duke
import DukeScenes as DukeScenes
import InitialTown as IT
import Control.Monad (when)



main = do
    Termios.set_conio_terminal_mode 0
    Termios.loadSprites 0
    run_state IT.newState
    Termios.reset_terminal_mode 0


run_state state = do
    Termios.drawScene
        (Duke.getSprite state)
        (Duke.getText state)
    hFlush stdout
    c <- getChar
    (newState, shouldQuit) <- return (Duke.advanceState state c)
    if shouldQuit
        then return ()
        else run_state newState
