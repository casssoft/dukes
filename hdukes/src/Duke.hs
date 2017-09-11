{-# LANGUAGE ForeignFunctionInterface #-}
module Duke where

import Foreign.C.Types
--import Data.Map (Map)
--import qualified Data.Map as Map
import Data.List -- intercalate


data Sprite = Duke | King | Knight deriving Show

--data State = State CInt String


spritesToNum :: Sprite -> CInt
spritesToNum Duke = 0
spritesToNum King = 1
spritesToNum Knight = 2
spritesToNum x = error ("Bad sprite given " ++ show x)

data Display = Display {
    dSprite :: Sprite,
    dText :: String
}

data Scene = Text {
        teSprite :: Sprite,
        teText :: [String] --lines
    } |
    Choice {
        cSprite :: Sprite,
        cText :: [String],
        cResolver :: State -> Char -> State
    } |
    Transition {
        trSprite :: Sprite,
        trText :: [String], -- lines
        trResolver :: State -> State
    }


sceneWithSprite :: Sprite -> [[String]] -> [Scene]
sceneWithSprite sprite (x:xs) =
    (Text sprite x):(sceneWithSprite sprite xs)

intro = 
    (sceneWithSprite Duke
    [[
    "Hey it's you!",
    "...",
    "Come on press space!"],
    [
    "My name is Dukeling",
    "I'm sure you already know that though"],
    [
    "Oh you don't?"],
    [
    "Ahh I guess you are new here",
    "I'll introduce you to everyone"]]) ++
    [(Choice Duke [
    "Do you want me to introduce you to",
    "the Knight (1)? or the King (2)"]
    (\state x -> state)
    )]

data State = State {
    curScenes :: [Scene]
    }

getSprite :: State -> CInt
getSprite (State ((Text sprite text):xs)) = spritesToNum sprite
getSprite (State ((Choice { cSprite=sprite }):xs)) = spritesToNum sprite
getSprite (State ((Transition { trSprite=sprite }):xs)) = spritesToNum sprite

getText :: State -> String
getText (State ((Text { teText=text }):xs)) = formatText text
getText (State ((Choice { cText=text }):xs)) = formatText text
getText (State ((Transition { trText=text }):xs)) = formatText text
getText (State _) = error "getText on unknown Scene type"

formatText :: [String] -> String
formatText = intercalate "\n"


newState :: State
newState = (State intro)
advanceState :: State -> Char -> (State, Bool)
advanceState x 'q' = (x, True)
advanceState state@(State {curScenes=slist}) x = processScenes slist state x

processScenes :: [Scene] -> State -> Char -> (State, Bool)

processScenes (Text {}:xs) state ch
    | ch == ' ' = (state { curScenes=xs }, False)
    | otherwise = (state, False)
