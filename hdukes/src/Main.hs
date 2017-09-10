module Main where
import Control.Exception
import System.IO
import Termios as Termios

import Control.Monad (when)
--import Data.Map (Map)
--import qualified Data.Map as Map
--
--data Sprite = Duke | King | Knight
--
--spritesToNum :: Sprites -> Number
--spritesToNum Duke = 0
--spritesToNum King = 1
--spritesToNum Knight 2
--spritesToNum x = error ("Bad sprite given " ++ show x)
--
--data Display = Display {
--    dSprite :: Sprite,
--    dText :: String
--}
--
--data Scene = Text {
--    tSprite :: tSprite,
--    tText :: [String]
--    } |
--    Choice {
--        cSprite :: Sprite
--        cText :: String,
--        cResolver :: State -> Char -> State
--    } |
--    Transition {
--        tResolver :: State -> State
--    }
--
--
--
--data State = State {
--    curSprite



main = do
    Termios.set_conio_terminal_mode 0
    Termios.loadSprites 0
    (while_true prompt)
    Termios.reset_terminal_mode 0

while_true op = do
    continue <- op
    when continue (while_true op)

prompt = do
    putStr "? "
    hFlush stdout
    c <- getChar
    --putStrLn $ "you typed " ++ [c]
    Termios.drawScene 0 "hello"
    return (c /= 'q')

