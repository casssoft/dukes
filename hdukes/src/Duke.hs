{-# LANGUAGE ForeignFunctionInterface #-}
module Duke where

import Foreign.C.Types
--import Data.Map (Map)
--import qualified Data.Map as Map
import Data.List -- intercalate


data Sprite = Duke | King | Knight | Zach deriving Show

--data State = State CInt String


spritesToNum :: Sprite -> CInt
spritesToNum Duke = 0
spritesToNum King = 1
spritesToNum Knight = 2
spritesToNum Zach = 3
spritesToNum x = error ("Bad sprite given " ++ show x)

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
sceneWithSprite sprite [] = []



chooseWithOptions :: [(Char, [Scene])] -> State -> Char -> State
chooseWithOptions [] state ch = state
chooseWithOptions ((opch, scenes):xs) state@(State {curScenes=(choice:cscenes)})  ch
    | opch == ch = state {curScenes=(cscenes ++ scenes)}
    | otherwise = chooseWithOptions xs state ch

gotoTransition :: [Scene] -> State -> State
gotoTransition newstates state@(State {curScenes=(transition:cscenes)}) =
    state {curScenes=(cscenes ++ newstates)}

removeCurrentScene :: State -> State
removeCurrentScene state@(State {curScenes=(curscene:xs)}) =
    state {curScenes=xs}

zachintro =
    (sceneWithSprite Zach
    [[
    "With a great beard",
    "Comes great responsibility"]])
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
    "the Knight (1)? or the King (2) or Zach10 (3)"]
    (chooseWithOptions [('1', knightintro), ('2', kingintro), ('3', zachintro)])
    )] ++
    [(Text Duke [
    "Oh ok, they are right over there"])]

kingintro =
    (sceneWithSprite King
    [[
    "Yo it's me the King!"],
    [
    "I'm very busy right now goodbye"]]) ++
    [(Transition King ["Please talk to Duke again"] (gotoTransition intro))]

knightintro =
    (sceneWithSprite Knight
    [[
    "Yes man I am the Knight..."],
    [
    "Are you hungry?",
    "I have a cookie I can give you"]]) ++
    [(Transition Knight ["Here's your cookie!"]
        (removeCurrentScene .
        (\state@(State {sCookies=cookies}) ->
            state{sCookies=(cookies + 1)})))]

data State = State {
    curScenes :: [Scene],
    sCookies :: Integer
    }

getSprite :: State -> CInt
getSprite (State {curScenes=((Text sprite text):xs)}) = spritesToNum sprite
getSprite (State {curScenes=((Choice { cSprite=sprite }):xs)}) = spritesToNum sprite
getSprite (State {curScenes=((Transition { trSprite=sprite }):xs)}) = spritesToNum sprite
getSprite (State {curScenes=[]}) = 0

getText :: State -> String
getText (State {curScenes=((Text { teText=text }):xs)}) = formatText text
getText (State {curScenes=((Choice { cText=text }):xs)}) = formatText text
getText (State {curScenes=((Transition { trText=text }):xs)}) = formatText text
getText (State {curScenes=[], sCookies=cookies}) = "GAME OVER cookies: " ++ (show cookies)
getText (State {}) = error "getText on unknown Scene type"

formatText :: [String] -> String
formatText = intercalate "\n"


newState :: State
newState = (State intro 0)
advanceState :: State -> Char -> (State, Bool)
advanceState x 'q' = (x, True)
advanceState state@(State {curScenes=slist}) x = processScenes slist state x

processScenes :: [Scene] -> State -> Char -> (State, Bool)

processScenes (Text {}:xs) state ch
    | ch == ' ' = (state { curScenes=xs }, False)
    | otherwise = (state, False)

processScenes [] state ch = (state, True)

processScenes (Choice {cResolver=resolvefunc}:xs) state ch =
    ((resolvefunc state ch), False)

processScenes (Transition {trResolver=resolvefunc}:xs) state ch
    | ch == ' ' = ((resolvefunc state), False)
    | otherwise = (state, False)
