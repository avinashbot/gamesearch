module Main (main) where

import Data.Maybe                (fromJust, isJust)
import Game.GameSearch           (Spec (..), bestAction, empty, timedMCTS)
import Game.GameSearch.Oware     (Board (..), Action (..), start)
import System.IO                 (hFlush, stdout)
import System.Random             (getStdGen)

main :: IO ()
main = do
    rand      <- getStdGen
    finalNode <- timedMCTS 5 rand state empty
    print (actions state)
    print (scores state)
    print state
    print (bestAction finalNode state)
    where
        state =
            apply (Action 6) $
            apply (Action 4) $
            apply (Action 11) $
            apply (Action 5) $
            apply (Action 7) $
            apply (Action 3) $
            apply (Action 7) $
            apply (Action 2) $
            apply (Action 11) $
            apply (Action 5) $
            apply (Action 10) $
            apply (Action 1) $
            apply (Action 11) $
            apply (Action 0) $
            apply (Action 8) $
            apply (Action 1) $
            apply (Action 11) $
            apply (Action 4) $
            apply (Action 10) $
            apply (Action 0) $
            apply (Action 9) $
            apply (Action 0) $
            apply (Action 10) $
            apply (Action 2) $
            apply (Action 11) $
            apply (Action 1) $
            apply (Action 6) $
            apply (Action 1) $
            apply (Action 11) $
            apply (Action 5) $
            apply (Action 7) $
            apply (Action 4) $
            apply (Action 8) $
            apply (Action 2) $
            apply (Action 11) $
            apply (Action 0) $
            apply (Action 10) $
            apply (Action 5) $
            apply (Action 11) $
            apply (Action 3) start
