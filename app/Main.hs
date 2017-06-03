module Main where

import Control.Monad
import System.IO                (hFlush, stdout)
import System.Random            (StdGen, getStdGen)
import Data.Maybe               (isJust, fromJust)
import Game.Fishmax             (Spec(..), monteCarlo, emptyNode, best, apply)
import Game.Fishmax.ConnectFour (State(..), Drop(..), start)

main :: IO ()
main = continue start

continue :: State -> IO ()
continue state = do
    rand <- getStdGen
    let action = fst (best (fst (runMCTS rand state 5e5))) :: Drop
    let computerMove = apply action state
    putStrLn ("Computer Chose: " ++ show action)
    if   isJust (winner computerMove)
    then putStrLn "Computer Wins"
    else do
         putStr "Player Move (0-6): "
         hFlush stdout
         uinput <- getLine
         let playerMove = apply (Drop (read uinput)) computerMove
         if   isJust (winner playerMove)
         then putStrLn "Player Wins"
         else continue playerMove

runMCTS r s 0 = monteCarlo r s emptyNode
runMCTS r s i = monteCarlo pr s pn where (pn, (pr, _)) = runMCTS r s (i - 1)
