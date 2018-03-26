{-# LANGUAGE ForeignFunctionInterface #-}
module Duke where

import Foreign.C.Types
--import Data.Map (Map)
--import qualified Data.Map as Map
import Data.List -- intercalate
import Items as Items
import qualified Data.Set as Set



data Sprite =
    NoSprite |
    Duke |
    Royal |
    Knight |
    Zach |
    TownSquare |
    Forrest |
    InventorySprite |
    TitleScreen |
    Lake |
    FeetInLake |
    Clearing |
    Chimo |
    ClearingWithChimo |
    TinyHutInside |
    BigHouseInside |
    Jerry |
    ForrestWithHole |
    Dark deriving Show


spritesToNum :: Sprite -> String
spritesToNum NoSprite = ""
spritesToNum Duke = "dukeofvim"
spritesToNum Royal = "royal"
spritesToNum Knight = "knight"
spritesToNum Zach = "zach"
spritesToNum TownSquare = "townsquare"
spritesToNum Forrest = "forrest"
spritesToNum InventorySprite = "inventory"
spritesToNum TitleScreen = "intro"
spritesToNum Lake = "lake"
spritesToNum FeetInLake = "feetinlake"
spritesToNum Clearing = "clearing"
spritesToNum Chimo = "chimo"
spritesToNum ClearingWithChimo = "clearingwithchimo"
spritesToNum TinyHutInside = "tinyhutinside"
spritesToNum BigHouseInside = "bighouseinside"
spritesToNum Jerry = "jerry"
spritesToNum ForrestWithHole = "forrestwithhole"
spritesToNum Dark = "dark"
--spritesToNum x = error ("Bad sprite given " ++ show x)

data Scene =
    Text { -- Display text
        teSprite :: Sprite,
        teText :: [String] --lines
    } |
    Choice { -- Display text and do something based on char
        cSprite :: Sprite,
        cText :: [String],
        cResolver :: State -> Char -> State
    } |
    Transition { -- Display and change the state
        trSprite :: Sprite,
        trText :: [String], -- lines
        trResolver :: State -> State
    } |
    GenericScene { -- Everything dependent on state (do whatever)
        gSprite :: State -> Sprite,
        gText :: State -> [String],
        gResolver :: State -> Char -> State
    } |
    MetaMenuScene { -- Do whatever but don't be interrupted by 'i' or 'h' (for inv and help menus)
        mmSprite :: State -> Sprite,
        mmText :: State -> [String],
        mmResolver :: State -> Char -> State
    }


data State = State {
    curScenes :: [Scene],
    sInv :: Items.Inventory,
    hasDone :: Set.Set String -- TODO String or enum?
}

stateHasDone :: String -> State -> Bool
stateHasDone thing (State {hasDone=doneset}) =
    Set.member thing doneset

setHasDone :: String -> State -> State
setHasDone thing state@(State {hasDone=doneset}) =
    state { hasDone=( Set.insert thing doneset )}


inventoryscene =
    (MetaMenuScene
        (\s -> InventorySprite)
        (\(State {sInv=inv}) ->
            "Here are your items:" :
            "-----------------" :
            (Items.showItems inv) ++
            ["-----------------"])
        continueWithSpace)

titlescreen =
    (MetaMenuScene
        (\s -> TitleScreen)
        (\s -> [ "Press space to continue!" ])
        continueWithSpace)

debugscreen =
    (MetaMenuScene
        (\s -> NoSprite)
        (\s -> Set.elems (hasDone s))
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

chooseNPC :: [(Char, State -> [Scene])] -> State -> Char -> State
chooseNPC [] state ch = state
chooseNPC ((opch, npcfunc):xs) state@(State {curScenes=(choice:cscenes)}) ch
    | opch == ch = state {curScenes=(cscenes ++ (npcfunc state))}
    | otherwise = chooseNPC xs state ch

gotoTransition :: [Scene] -> State -> State
gotoTransition newstates state@(State {curScenes=(transition:cscenes)}) =
    state {curScenes=(cscenes ++ newstates)}

switchOnHasDone :: [(String, [Scene])] -> State -> State
switchOnHasDone [] state = state
switchOnHasDone ((doneThing, scenes):xs) state@(State {curScenes=(thisscene:curScenes)})
    | doneThing == "" = state {curScenes=(scenes ++ curScenes)}
    | stateHasDone doneThing state = state {curScenes=(scenes ++ curScenes)}
    | otherwise = switchOnHasDone xs state

switchScenes :: [(State -> Bool, [Scene])] -> State -> State
switchScenes [] state = state
switchScenes ((boolfunc, scenes):xs) state@(State {curScenes=(thisscene:curScenes)})
    | boolfunc state = state {curScenes=(scenes ++ curScenes)}
    | otherwise = switchScenes xs state

removeCurrentScene :: State -> State
removeCurrentScene state@(State {curScenes=(curscene:xs)}) =
    state {curScenes=xs}

addItemToState :: Items.Item -> State -> State
addItemToState item state@(State {sInv=inv}) =
    state { sInv=(Items.addItem inv item)}

getSprite :: State -> String
getSprite (State {curScenes=((Text sprite text):xs)}) = spritesToNum sprite
getSprite (State {curScenes=((Choice { cSprite=sprite }):xs)}) = spritesToNum sprite
getSprite (State {curScenes=((Transition { trSprite=sprite }):xs)}) = spritesToNum sprite
getSprite state@(State {curScenes=((GenericScene { gSprite=spritefn }):xs)}) = spritesToNum (spritefn state)
getSprite state@(State {curScenes=((MetaMenuScene { mmSprite=spritefn }):xs)}) = spritesToNum (spritefn state)
getSprite (State {curScenes=[]}) = ""

getText :: State -> String
getText (State {curScenes=((Text { teText=text }):xs)}) = formatText text
getText (State {curScenes=((Choice { cText=text }):xs)}) = formatText text
getText (State {curScenes=((Transition { trText=text }):xs)}) = formatText text
getText state@(State {curScenes=((GenericScene { gText=textfn }):xs)}) = formatText (textfn state)
getText state@(State {curScenes=((MetaMenuScene { mmText=textfn }):xs)}) = formatText (textfn state)
getText (State {curScenes=[], sInv=inv}) = "GAME OVER cookies: " ++ (show (Items.countItem inv Items.Cookie))
--getText (State {}) = error "getText on unknown Scene type"

formatText :: [String] -> String
formatText = intercalate "\n"


advanceState :: State -> Char -> (State, Bool)
advanceState x 'q' = (x, True)
advanceState state@(State {curScenes=[]}) x = (state, True)
advanceState state@(State {curScenes=slist}) x = (processScenes slist state x, False)

processScenes :: [Scene] -> State -> Char -> State

-- first handle meta menus
processScenes (MetaMenuScene {mmResolver=resolvefunc}:xs) state ch =
    resolvefunc state ch

-- spawn meta menus
processScenes slist state 'i' =
    state {curScenes=(inventoryscene : slist)}

processScenes slist state 'h' =
    state {curScenes=(titlescreen : slist)}

processScenes slist state 'd' =
    state {curScenes=(debugscreen : slist)}

-- do everything else
processScenes (Text {}:xs) state ch
    | ch == ' ' = state { curScenes=xs }
    | otherwise = state

processScenes (Choice {cResolver=resolvefunc}:xs) state ch =
    resolvefunc state ch

processScenes (Transition {trResolver=resolvefunc}:xs) state ch
    | ch == ' ' = resolvefunc state
    | otherwise = state

processScenes (GenericScene {gResolver=resolvefunc}:xs) state ch =
    resolvefunc state ch

--processScenes [] state ch = state Should never happen, since we pattern match it out in advance State

