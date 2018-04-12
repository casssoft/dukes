{-# LANGUAGE ForeignFunctionInterface #-}
module DukeScenes where

import Foreign.C.Types
--import Data.Map (Map)
--import qualified Data.Map as Map
import Data.List -- intercalate
import Items as Items
import Duke
import qualified Data.Set as Set
import Debug.Trace


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

startTalkingScene :: Sprite -> [String] -> String -> Scene
startTalkingScene sprite dialog person =
    (Transition sprite dialog (
    (\s -> s{curScenes=(if stateHasDone ("met" ++ person) s 
        then hasMetScenes person
        else notMetScenes person)})))


notMetScenes :: String -> [Scene]
hasMetScenes :: String -> [Scene]

notMetScenes p
    | p == "Duke" = dukeintro
    | p == "Knight" = knightintro
    | p == "Royal" = royalintro
    | p == "Zach" = zachintro
    | p == "Chimo" = chimointro
    | p == "Jerry" = jerryintro

hasMetScenes p
    | p == "Duke" = dukeNormal
    | p == "Knight" = knightNormal
    | p == "Royal" = royalNormal
    | p == "Zach" = zachintro
    | p == "Chimo" = chimoInClearing
    | p == "Jerry" = jerrytalktoagain

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
    (Transition Forrest ["You are lost in the forrest.."]
        (setHasDone "seenForrest" . removeCurrentScene)) :
    (sceneWithSprite Forrest
    [
    ["...",
    "You are stil lost in the forrest"]]) ++
    [(Choice Forrest
        ["(1) Go to the left",
        "(2) Go straight",
        "(3) Go to the right",
        "(4) Try to find your way back"]
        (chooseWithOptions [
            ('1', stumbletolake),
            ('2', stumbletoclearing),
            ('3', forrestwithhole),
            ('4', townsquare)]))]


stumbletolake =
    [(Text Forrest
    ["Is that water?"]),
    (Text Lake ["You stumbled onto a pristine lake..."]),
    (Choice Lake ["Do you want to dip your feet in the water?",
                    "(1) Yes (2) No"]
    (chooseWithOptions [
        ('1', feetinwater),
        ('2', lakescene)]))]

forrestwithhole =
    [(Choice ForrestWithHole
        ["There is a hole next to the road",
        "(1) Jump in hole (2) Keep going"]
        (chooseWithOptions [
            ('1', insideforresthole),
            ('2', lostatforrest)
        ]))]

insideforresthole =
    [(Transition Dark ["It's very dark in here..."]
        (switchScenes [
            ((stateHasDone "givenCookieAndUnlockPortal", insidehole_choose)),
            ((\s -> True), [startTalkingScene Dark ["I hear footsteps!"] "Jerry"])]))]

insidehole_choose =
    [(Choice Dark
        ["(1) Talk to Jerry (2) Keep going"]
        (chooseWithOptions [
            ('1', (hasMetScenes "Jerry")),
            ('2', deeperintothehole)]))]

deeperintothehole = []

lakescene =
    [ (Choice Lake ["What a pristine lake...", "(1) Walk back (2) Walk around the lake (3) Dip feet"]
    (chooseWithOptions [
        ('1', lostatforrest),
        ('2', walkaroundthelake), -- TODO
        ('3', feetinwater)]
        ))]


foundyourwayback =
    [(Transition Forrest ["I think the town was this way..."]
    (ifHasDone "seenClearing"
        (gotoTransition clearingwithchimo_huh)
        (setHasDone "seenClearing" . gotoTransition clearing_huh)))]

stumbletoclearing =
    [(Transition Forrest ["I hope this is the right direction..."]
    (ifHasDone "seenClearing"
        (gotoTransition clearingwithchimo_huh)
        (setHasDone "seenClearing" . gotoTransition clearing_huh)))]
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
    "Yeah I'm just hanging out here alone"],
    ["bye"]]) ++
    [(Transition Lake ["I'll just keep walking"]
        (gotoTransition lakescene))]

royallakeintro =
    (sceneWithSprite Royal
    [[
    "Hello I'm Royal",
    "..."],
    ["Yeah I'm just hanging out here alone"],
    ["bye"]]) ++
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
        ('2', (\s -> lostatforrest))]))]

clearing =
    [(Choice Clearing ["It's a clearing in the trees.", "(1) Look around (2) Continue walking"]
    (chooseWithOptions [
        ('1', lookaroundclearing),
        ('2', lostatforrest)]))]

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
    [(Transition Duke
    [
    "Hey!",
    "Did you meet everyone yet?"]
    (switchOnHasDone [
        ("metChimo",
            [(Text Duke ["Who's Chimo?"])]),
        ("metRoyal",
            [(Text Duke ["Oh you met the Royal?", "Yeah he can be rude sometimes."])]),
        ("metKnight",
            [(Text Duke ["I heard there was a nice lake somewhere in the forrest"])]),
        ("",
            [(Text Duke ["You should try exploring a little bit."])])
    ]))] ++ townsquare

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
    "My name is Dukeling.",
    "I'm sure you already know that though."],
    [
    "Oh you don't?"],
    [
    "Ahh I guess you are new here.",
    "Welcome to Dukesville!"],
    [
    "This town was founded in 1337 by ..."],
    [
    "HEY!!!!!"],
    [
    "Did you fall asleep?",
    "That's very rude."],
    [
    "I guess you will never know the",
    "long and detailed history of Dukesville."],
    ["Anyhow..."]]) ++
    [(Transition Duke
    [
    "You should introduce yourself to everyone,",
    "but remember not to be rude!"]
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

-- Jerry

jerryintro =
    (sceneWithSprite Jerry
    [[
    "Yo what's up?"],
    ["Yeah it's me Jerry."]]) ++
    [(Transition Jerry ["Nice to meet you."]
        (setHasMet "Jerry" . removeCurrentScene))] ++
        jerrycaptialismtalk


jerrytalktoagain =
    [(Transition Jerry ["Yo it's you again"]
        (switchScenes [
            ((hasItemInState Items.Cookie), jerryhascookies),
            ((\s -> True), jerrybeeninthishole)])
    )]

jerrybeeninthishole =
    (sceneWithSpriteThenTransition Jerry
    [[
    "Hmm maybe I've been in this hole too long?"],
    [
    "Does it still work like that out there?"]]
    (gotoTransition forrestwithhole))

jerrycaptialismtalk =
    (sceneWithSprite Jerry
    [[
    "I don't get how they manage to trick us so bad."],
    [
    "Why can't the farmer afford fresh produce?"],
    [
    "And why can't the builder afford a house?"],
    [
    "The teacher has to get a loan to",
    "send their kids to college..."],
    [
    "The flight attendent can't afford to",
    "take a vacation..."],
    [
    "I used to be a baker but I could",
    "never afford a cookie..."]]) ++
    [(Transition Jerry
    [
    "I think something's wrong"]
    (gotoTransition forrestwithhole))]

jerryhascookies =
    [(Choice Jerry [
    "Oh you have a cookie..",
    "Can I have it?",
    "",
    " (Y) Yes (N) No"]
    (chooseWithOptions [
        ('y', jerrygivecookie),
        ('n', jerryrefusecookie)
        ])
    )]

jerrygivecookie =
    (Transition Jerry ["* munch munch munch *"]
    (removeCurrentScene .
    removeItemInState Items.Cookie)) :
    (sceneWithSpriteThenTransition Jerry
    [
    [
    "Yo that was actually not that tasty."],
    [
    "A ton of sugar too... Gross."],
    [
    "But thank you so much for the cookie!"],
    ["Now I know how it feels to eat cookies..."],
    ["..."],
    ["Oh btw this hole leads to the Other town..."]]
    (setHasDone "givenCookieAndUnlockPortal" .
    gotoTransition insideforresthole))
-- How should I let the player go further???

jerryrefusecookie =
    (sceneWithSprite Jerry
    [
    [
    "Oh no problem..."]]) ++
    forrestwithhole
