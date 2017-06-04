module Main (main) where

import Control.Monad
import System.CPUTime           (getCPUTime)
import System.IO                (hFlush, stdout)
import System.Random            (getStdGen)
import Data.Maybe               (isJust, fromJust)
import Game.Fishmax             (timedMCTS, emptyNode, bestAction, apply)
import Game.Fishmax.ConnectFour (State, Drop(..), start, winner)

main :: IO ()
main = continue start

continue :: State -> IO ()
continue state = do
    -- Run MCTS for 5 seconds.
    rand      <- getStdGen
    finalNode <- timedMCTS 5 rand state emptyNode

    -- Pick the move with the best outcome.
    let computerAction = fst (bestAction finalNode)
    let computerState = apply computerAction state

    -- Put computer move and leave if it's a winning move.
    putStrLn ("Computer Chose: " ++ show computerAction)
    if   isJust (winner computerState)
    then putStrLn "Computer Wins"
    else do
         -- Ask player for their move.
         putStr "Player Move (0-6): "
         hFlush stdout
         uinput <- getLine

         -- Check if it's a winning move, else go back.
         let playerState = apply (Drop (read uinput)) computerState
         if   isJust (winner playerState)
         then putStrLn "Player Wins"
         else continue playerState
