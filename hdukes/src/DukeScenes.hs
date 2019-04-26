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
