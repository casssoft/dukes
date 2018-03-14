{-# LANGUAGE ForeignFunctionInterface #-}
module Duke where

import Foreign.C.Types
--import Data.Map (Map)
--import qualified Data.Map as Map
import Data.List -- intercalate
import Items as Items


data Sprite = NoSprite | Duke | Royal | Knight | Zach | TownSquare | Forrest | InventorySprite | TitleScreen deriving Show

--data State = State CInt String


spritesToNum :: Sprite -> CInt
spritesToNum NoSprite = -1
spritesToNum Duke = 0
spritesToNum Royal = 1
spritesToNum Knight = 2
spritesToNum Zach = 3
spritesToNum TownSquare = 4
spritesToNum Forrest = 5
spritesToNum InventorySprite = 6
spritesToNum TitleScreen = 7
--spritesToNum x = error ("Bad sprite given " ++ show x)

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
    } |
    GenericScene {
        gSprite :: State -> Sprite,
        gText :: State -> [String],
        gResolver :: State -> Char -> State
    }


townSquareShowCookies :: Scene
townSquareShowCookies =
    (GenericScene
        (\s -> TownSquare)
        (\(State {sInv=inv}) ->
            "Here are your items:" :
            (Items.showItems inv))
        continueWithSpace)

inventoryscene =
    (GenericScene
        (\s -> InventorySprite)
        (\(State {sInv=inv}) ->
            "Here are your items:" :
            (Items.showItems inv))
        continueWithSpace)

continueWithSpace :: State -> Char -> State
continueWithSpace state ch
    | ch == ' ' = removeCurrentScene state
    | otherwise = state

--getText (State {curScenes=[], sCookies=cookies}) = "GAME OVER cookies: " ++ (show cookies)


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


titlescreen =
    (sceneWithSprite TitleScreen
    [[
    "Press ' ' to continue!"
    ]])

townsquare =
--    (sceneWithSprite TownSquare
--    [[
--    ]) ++
    [(Choice TownSquare [
    "Welcome to Dukesvile!",
    "",
    "Who do you want to visit?",
    "the Duke (1) or the Knight (2) or the Royal (3) or Zach10 (4)"]
    (chooseWithOptions [('1', dukeintro), ('2', knightintro), ('3', kingintro), ('4', zachintro)])
    )]


lostatforrest =
    (sceneWithSprite Forrest
    [[
    "You are lost in the forrest.."],
    ["..."],
    ["You are stil lost in the forrest"]])

zachintro =
    (sceneWithSprite Zach
    [[
    "With a great beard",
    "Comes great responsibility"]]) ++
    [(Transition Zach ["Goodbye"] (gotoTransition lostatforrest))]
dukeintro =
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
    "the Knight (1)? or the Royal (2) or Zach10 (3)"]
    (chooseWithOptions [('1', knightintro), ('2', kingintro), ('3', zachintro)])
    )] ++
    [(Text Duke [
    "Oh ok, they are right over there"])]

kingintro =
    (sceneWithSprite Royal
    [[
    "Yo it's me the Royal!"],
    [
    "I'm very busy right now"]]) ++
    [(Transition Royal ["Goodbye"] (gotoTransition townsquare))]

knightintro =
    (sceneWithSprite Knight
    [[
    "Yes man I am the Knight..."],
    [
    "Are you hungry?",
    "I have a cookie I can give you"]]) ++
    [(Transition Knight ["Here's your cookie!"]
        ((\state@(State {sInv=inv}) ->
            state{sInv=(Items.addItem inv Items.Cookie)}) . gotoTransition townsquare))]



data State = State {
    curScenes :: [Scene],
    sInv :: Items.Inventory
    }

getSprite :: State -> CInt
getSprite (State {curScenes=((Text sprite text):xs)}) = spritesToNum sprite
getSprite (State {curScenes=((Choice { cSprite=sprite }):xs)}) = spritesToNum sprite
getSprite (State {curScenes=((Transition { trSprite=sprite }):xs)}) = spritesToNum sprite
getSprite state@(State {curScenes=((GenericScene { gSprite=spritefn }):xs)}) = spritesToNum (spritefn state)
getSprite (State {curScenes=[]}) = -1

getText :: State -> String
getText (State {curScenes=((Text { teText=text }):xs)}) = formatText text
getText (State {curScenes=((Choice { cText=text }):xs)}) = formatText text
getText (State {curScenes=((Transition { trText=text }):xs)}) = formatText text
getText (State {curScenes=[], sInv=inv}) = "GAME OVER cookies: " ++ (show (Items.countItem inv Items.Cookie))
getText state@(State {curScenes=((GenericScene { gText=textfn }):xs)}) = formatText (textfn state)
--getText (State {}) = error "getText on unknown Scene type"

formatText :: [String] -> String
formatText = intercalate "\n"


newState :: State
newState = (State (titlescreen ++ townsquare) [])
advanceState :: State -> Char -> (State, Bool)
advanceState x 'q' = (x, True)
advanceState state@(State {curScenes=xs}) 'i' = (state { curScenes=(inventoryscene:xs) }, False)
advanceState state@(State {curScenes=xs}) 'h' = (state { curScenes=(titlescreen ++ xs) }, False)
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

processScenes (GenericScene {gResolver=resolvefunc}:xs) state ch =
    ((resolvefunc state ch), False)
