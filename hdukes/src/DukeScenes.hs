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
    | p == "Chimo" = chimointro

hasMetScenes p
    | p == "Duke" = dukeNormal
    | p == "Knight" = knightNormal
    | p == "Royal" = royalNormal
    | p == "Zach" = zachintro
    | p == "Chimo" = chimoInClearing

setHasMet :: String -> State -> State
setHasMet person = setHasDone ("met" ++ person)

ifHasMet :: String -> (State -> State) -> (State -> State) -> State -> State
ifHasMet person func1 func2 state =
    if stateHasDone ("met" ++ person) state
        then func1 state
        else func2 state

ifHasDone :: String -> (State -> State) -> (State -> State) -> State -> State
ifHasDone thing func1 func2 state =
    if stateHasDone thing state
        then func1 state
        else func2 state

-- PLACES --
townsquare =
    [(Choice TownSquare [
    "Welcome to Dukesvile!",
    "",
    "There's someone standing next to the road.",
    "  (1) Talk to the Duke",
    "",
    "There's some houses in the town square.",
    "  (2) Walk into the tiny cottage",
    "  (3) Walk into the big house",
    "",
    "To the left there are some trees.",
    "  (4) Walk into the trees"]
    (chooseNPC [
        ('1', talkTo "Duke"),
        ('2', (\s -> tinyhut)),
        ('3', (\s -> bighouse)),
        ('4', (\s -> lostatforrest))])
    )]

tinyhut =
    [(Choice TinyHutInside [
    "This is a pretty cute cottage!",
    "",
    "There is someone with a sword standing there!",
    " (1) Talk to the Knight",
    " (2) Walk outside"]
    (chooseNPC [
        ('1', talkTo "Knight"),
        ('2', (\s -> townsquare))]))]

-- TODO add people in the big house
-- Zach10 is unused and royal only appears around the lake
bighouse =
    (sceneWithSprite BigHouseInside
    [[
    "Pretty big!"],
    ["..."]]) ++
    [(Transition BigHouseInside ["This is awkward I think I should leave."]
        (gotoTransition townsquare))]

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
    [(Transition Forrest ["I think the town was this way..."]
    (ifHasDone "seenClearingOnce"
        (gotoTransition clearingwithchimo_huh)
        (setHasDone "seenClearingOnce" . gotoTransition clearing_huh)))]

clearingwithchimo_huh =
    (Text ClearingWithChimo ["Huh a grassy clearing..."])
    -- becareful with infinite list!
        : clearingwithchimo

clearing_huh =
    (Text Clearing ["Huh a grassy clearing..."])
        : clearing


walkaroundthelake =
    [(Text Lake ["Hmm this lake looks the same from this angle too"]),
    (Transition Lake ["Who's that?"]
        (ifHasMet "Royal" (gotoTransition royallakemet) (gotoTransition royallakeintro)))]

royallakemet =
    (sceneWithSprite Royal
    [[
    "* ehh who is it now > _ > *"],
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

clearingwithchimo =
    [(Choice ClearingWithChimo ["It's a clearing in the trees.", "(1) Talk to Chimo (2) Continue walking"]
    (chooseNPC [
        ('1', talkTo "Chimo"),
        ('2', (\s -> townsquare))]))]

clearing =
    [(Choice Clearing ["It's a clearing in the trees.", "(1) Look around (2) Continue walking"]
    (chooseWithOptions [
        ('1', lookaroundclearing),
        ('2', townsquare)]))]

lookaroundclearing =
    (sceneWithSprite Clearing
    [[
    "Hmm an empty clearing..."],
    ["The grass looks like it's growing greener on this side."]]) ++
    [(Transition Clearing ["I think I can see the town from here"]
        (gotoTransition clearing))]
 

-- PEOPLE --
zachintro =
    (sceneWithSprite Zach
    [[
    "With a great beard",
    "Comes great responsibility"]]) ++
    [(Transition Zach ["Goodbye"] (gotoTransition townsquare))]

dukeNormal =
    (sceneWithSprite Duke --TODO if you actually met everyone do something here
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
    "Oh you don't?"],
    [
    "Ahh I guess you are new here",
    "Welcome to Dukesville!"]]) ++
    [(Transition Duke
    ["There's a bunch of interesting people that live around here.",
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
    (gotoTransition tinyhut))]

knightintro =
    (sceneWithSprite Knight
    [[
    "Yes man I am the Knight..."],
    [
    "Are you hungry?",
    "I have a cookie I can give you"]]) ++
    [(Transition Knight ["Here's your cookie!"]
    (addItemToState Items.Cookie
    . gotoTransition tinyhut
    . setHasMet "Knight"))]

-- Chimo npc

chimointro =
    (sceneWithSprite Chimo
    [[
    "h e l l o"],
    ["do i know you?",
    "..."],
    ["i guess not.."]]) ++
    [(Transition ClearingWithChimo ["Hmm..."]
        (setHasMet "Chimo"
        . gotoTransition clearingwithchimo))]

chimoInClearing =
    (sceneWithSprite Chimo
    [[
    "h e y"],
    ["i know you"],
    ["..."],
    ["my hands hurt",
    "..."],
    ["ive been staring at this stone all day"],
    ["i know its been moving",
    "..."],
    ["i cant seem to find the legs"]]) ++
    [(Transition ClearingWithChimo ["... I don't see a stone"]
        (gotoTransition clearingwithchimo))]
