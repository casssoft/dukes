{-# LANGUAGE ForeignFunctionInterface #-}
module DukeScenes where

import Foreign.C.Types
--import Data.Map (Map)
--import qualified Data.Map as Map
import Data.List -- intercalate
import Items as Items
import Duke
import qualified Data.Set as Set


newState :: State

newState = (State
    (titlescreen : townsquare) -- title then town
    [] -- empty inventory
    Set.empty -- empty hasDone
    )

-- NPC met helper logic
talkTo :: String -> State -> [Scene]
talkTo person state =
    if stateHasDone ("met" ++ person) state
        then hasMetScenes person
        else notMetScenes person


notMetScenes :: String -> [Scene]
hasMetScenes :: String -> [Scene]

notMetScenes p
    | p == "Duke" = dukeintro
    | p == "Knight" = knightintro
    | p == "Royal" = royalintro
    | p == "Zach" = zachintro

hasMetScenes p
    | p == "Duke" = dukeNormal
    | p == "Knight" = knightNormal
    | p == "Royal" = royalNormal
    | p == "Zach" = zachintro

setHasMet :: String -> State -> State
setHasMet person = setHasDone ("met" ++ person)

ifHasMet :: String -> (State -> State) -> (State -> State) -> State -> State
ifHasMet person func1 func2 state =
    if stateHasDone ("met" ++ person) state
        then func1 state
        else func2 state

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
        ('1', talkTo "Duke"),
        ('2', talkTo "Knight"),
        ('3', talkTo "Royal"),
        ('4', talkTo "Zach"),
        ('f', (\s -> lostatforrest))])
--, ('2', knightintro), ('3', kingintro), ('4', zachintro)])
    )]


lostatforrest =
    (sceneWithSprite Forrest
    [[
    "You are lost in the forrest.."],
    ["..."],
    ["You are stil lost in the forrest"],
    ["Is that water?"]]) ++
    [(Text Lake ["You stumbled onto a pristine lake..."]),
    (Choice Lake ["Do you want to dip your feet in the water?",
                    "(1) Yes (2) No"]
    (chooseWithOptions [
        ('1', feetinwater),
        ('2', lakescene)]))]

lakescene =
    [ (Choice Lake ["What a pristine lake...", "(1) Walk back (2) Walk around the lake (3) Dip feet"]
    (chooseWithOptions [
        ('1', foundyourwayback),
        ('2', walkaroundthelake), -- TODO
        ('3', feetinwater)]
        ))]


foundyourwayback =
    [(Text Forrest ["I think the town was this way..."]),
    (Transition Forrest ["Is that it over there?"]
        (gotoTransition townsquare))]

walkaroundthelake =
    [(Text Lake ["Hmm this lake looks the same from this angle too"]),
    (Transition Lake ["Who's that?"]
        (ifHasMet "Royal" (gotoTransition royallakemet) (gotoTransition royallakeintro)))]

royallakemet =
    (sceneWithSprite Royal
    [[
    "~Huh darn they found me~"],
    ["Heeey how are you doing?"],
    ["That's cool...",
    "Yeah I'm just hanging out here alone bye"]]) ++
    [(Transition Lake ["I'll just keep walking"]
        (gotoTransition lakescene))]

royallakeintro =
    (sceneWithSprite Royal
    [[
    "Hello I'm Royal",
    "..."],
    ["Yeah I'm just hanging out here alone bye"]]) ++
    [(Transition Lake ["That's rude... I'll just keep walking"]
        (setHasMet "Royal" . gotoTransition lakescene))] --todo should this count as a meeting?


feetinwater =
    [(Text FeetInLake ["You dip your feet in the cool water of the lake."]),
    (Transition FeetInLake
        ["It feels nice!",
        "You gain 1 relaxed point!",
        " . "]
        (addItemToState Items.RelaxedPoint .
        gotoTransition lakescene))]

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
        (removeCurrentScene . (setHasMet "Duke"))) :
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
    [(Text Royal
        ["Hey...",
        "I'm still pretty busy.."]),
     (Transition Royal
        ["Go play in the forrest or something"]
        (gotoTransition townsquare))]


royalintro =
    (sceneWithSprite Royal
    [[
    "Yo it's me the Royal!"],
    [
    "I'm very busy right now"]]) ++
    [(Transition Royal
    ["Goodbye"]
    (setHasMet "Royal" . gotoTransition townsquare))]



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
    (addItemToState Items.Cookie
    . gotoTransition townsquare
    . setHasMet "Knight"))]
