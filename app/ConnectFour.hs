module Main (main) where

import Control.Monad               ((<=<))
import Data.Maybe                  (isJust, fromJust)
import Game.GameSearch             (Node, apply, bestAction, empty, timedMCTS,
                                    child)
import Game.GameSearch.ConnectFour (State, Drop (..), Player, start, winner)
import System.IO                   (hFlush, stdout)
import System.Random               (getStdGen)

main :: IO ()
main = continue start empty

continue :: State -> Node Drop Player -> IO ()
continue state node = do
    -- Run MCTS for 5 seconds.
    rand      <- getStdGen
    finalNode <- timedMCTS 5 rand state empty

    -- Pick the move with the best outcome.
    let computerAction = bestAction finalNode state
    let computerState = apply computerAction state

    -- Put computer move and leave if it's a winning move.
    putStrLn ("Computer Chose: " ++ show computerAction)
    print computerState
    if   isJust (winner computerState)
    then putStrLn "Computer Wins"
    else do
        -- Ask player for their move.
        putStr "Player Move (0-6): "
        hFlush stdout
        uinput <- getLine

        -- Check if it's a winning move, else go back.
        let playerAction = Drop (read uinput)
        let playerState = apply playerAction computerState
        if   isJust (winner playerState)
        then putStrLn "Player Wins"
        else continue playerState $
             fromJust ((child playerAction <=< child computerAction) node)
