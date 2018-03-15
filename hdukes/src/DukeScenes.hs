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

-- NPC logic
talkTo :: Sprite -> State -> [Scene]
talkTo sprite state = npcToScene sprite (getNPC sprite state)
setHasMet :: Sprite -> State -> State
setHasMet sprite state = setNPC sprite state ((getNPC sprite state) { hasMet = True})

npcToScene :: Sprite -> NPCState -> [Scene]
npcToScene Duke (NPCState { hasMet= False }) =
    dukeintro
npcToScene Duke (NPCState { hasMet= True }) =
    dukeNormal

npcToScene Knight (NPCState { hasMet= True }) =
    knightNormal
npcToScene Knight (NPCState { hasMet = False }) =
    knightintro

npcToScene Royal (NPCState { hasMet = True }) =
    royalNormal

npcToScene Royal (NPCState { hasMet = False }) =
    royalintro

npcToScene Zach state =
    zachintro
--npcToScene Zach (NPCState { hasMet = True }) =
--    zachintro
--npcToScene Zach (NPCState { hasMet = False }) =
--    zachintro

townsquare =
    [(Choice TownSquare [
    "Welcome to Dukesvile!",
    "",
    "Who do you want to visit?",
    "the Duke (1) or the Knight (2) or the Royal (3) or Zach10 (4)",
    "",
    "Or you can visit the forrest (f)"]
    (chooseNPC [
        ('1', talkTo Duke),
        ('2', talkTo Knight),
        ('3', talkTo Royal),
        ('4', talkTo Zach),
        ('f', (\s -> lostatforrest))])
--, ('2', knightintro), ('3', kingintro), ('4', zachintro)])
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
    [(Transition Zach ["Goodbye"] (gotoTransition townsquare))]

dukeNormal =
    (sceneWithSprite Duke
    [[
    "Hey",
    "Did you meet everyone yet?"]]) ++ townsquare

dukeintro :: [Scene]
dukeintro =
    -- introduce duke
    (Transition Duke
        [
        "Hey it's you!",
        "...",
        "Come on press space!"]
        (removeCurrentScene . (setHasMet Duke))) :
    (sceneWithSprite Duke
    [[
    "My name is Dukeling",
    "I'm sure you already know that though"],
    [
    "Oh you don't?"]]) ++
    [(Transition Duke
    [
    "Ahh I guess you are new here",
    "You should introduce yourself to everyone!"]
    (gotoTransition townsquare))]
    --[(Choice Duke [
    --"Do you want me to introduce you to",
    --"the Knight (1)? or the Royal (2) or Zach10 (3)"]
    --(chooseWithOptions [('1', knightintro), ('2', royalintro), ('3', zachintro)])
    --)] ++
    --[(Text Duke [
    --"Oh ok, they are right over there"])]

-- Royal NPC
royalNormal =
    [(Transition Royal
        ["Hey...",
        "I'm still pretty busy.."]
        (gotoTransition townsquare))]


royalintro =
    (sceneWithSprite Royal
    [[
    "Yo it's me the Royal!"],
    [
    "I'm very busy right now"]]) ++
    [(Transition Royal
    ["Goodbye"]
    (setHasMet Royal . gotoTransition townsquare))]


-- Knight NPC

knightNormal =
    (sceneWithSprite Knight
    [[
    "Hey did you try that cookie I gave you?"]]) ++
    [(Transition Knight
    ["Oh...",
    "You should try it. It's pretty tasty"]
    (gotoTransition townsquare))]

knightintro =
    (sceneWithSprite Knight
    [[
    "Yes man I am the Knight..."],
    [
    "Are you hungry?",
    "I have a cookie I can give you"]]) ++
    [(Transition Knight ["Here's your cookie!"]
    ((\state@(State {sInv=inv}) ->
            state{sInv=(Items.addItem inv Items.Cookie)})
    . gotoTransition townsquare
    . setHasMet Knight))]
