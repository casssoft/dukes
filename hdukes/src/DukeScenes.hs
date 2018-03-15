{-# LANGUAGE ForeignFunctionInterface #-}
module DukeScenes where

import Foreign.C.Types
--import Data.Map (Map)
--import qualified Data.Map as Map
import Data.List -- intercalate
import Items as Items
import Duke


newState :: State

newState = (State
    (titlescreen : townsquare) -- title then town
    [] -- empty inventory
    emptyNPC -- duke
    emptyNPC -- knight
    emptyNPC -- royal
    emptyNPC -- zach
    )

townsquare =
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
