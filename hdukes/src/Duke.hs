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


emptyNPC = (NPCState False)

data NPCState = NPCState {
    haveMet :: Bool
}

data State = State {
    curScenes :: [Scene],
    sInv :: Items.Inventory,
    dukeState :: NPCState,
    knightState :: NPCState,
    royalState :: NPCState,
    zachState :: NPCState
}


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
        (\s -> [ "Press ' ' to continue!" ])
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



getSprite :: State -> CInt
getSprite (State {curScenes=((Text sprite text):xs)}) = spritesToNum sprite
getSprite (State {curScenes=((Choice { cSprite=sprite }):xs)}) = spritesToNum sprite
getSprite (State {curScenes=((Transition { trSprite=sprite }):xs)}) = spritesToNum sprite
getSprite state@(State {curScenes=((GenericScene { gSprite=spritefn }):xs)}) = spritesToNum (spritefn state)
getSprite state@(State {curScenes=((MetaMenuScene { mmSprite=spritefn }):xs)}) = spritesToNum (spritefn state)
getSprite (State {curScenes=[]}) = -1

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

