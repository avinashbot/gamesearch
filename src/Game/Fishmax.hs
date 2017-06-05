{-# LANGUAGE BangPatterns #-}

module Game.Fishmax
  ( module Game.Fishmax.TreeSearch
  , repeatMCTS
  , timedMCTS
  ) where

import Game.Fishmax.TreeSearch
import System.CPUTime (getCPUTime)
import System.Random (RandomGen)

-- Run MCTS a certain number of times.
repeatMCTS ::
     (RandomGen r, Spec s a) => Integer -> r -> s -> Node a -> (Node a, r)
repeatMCTS 0 r _ n = (n, r)
repeatMCTS 1 r s n = (nn, nr) where (nn, (nr, _)) = monteCarlo r s n
repeatMCTS i r s n = (nnn, nnr) where
  (nnn, (nnr, _)) = monteCarlo nr s nn
  (nn, nr)        = repeatMCTS (i-1) r s n

-- Run monte carlo search for a given number of seconds.
timedMCTS ::
     (RandomGen r, Spec s a) => Integer -> r -> s -> Node a -> IO (Node a)
timedMCTS seconds r s n = do
  curTime <- getCPUTime
  timedMCTS' (curTime + seconds * 1000000000000) r s n

-- Run monte carlo search until cpuTime hits a certain value.
timedMCTS' ::
     (RandomGen r, Spec s a) => Integer -> r -> s -> Node a -> IO (Node a)
timedMCTS' stopTime r s n = do
  let !(nn, (nr, _)) = monteCarlo r s n
  curTime <- getCPUTime
  if stopTime >= curTime
    then timedMCTS' stopTime nr s nn
    else return nn
