{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Game.GameSearch.Core
    ( Spec(..)
    , Node

    , empty
    , child
    , bestAction
    , monteCarlo
    ) where

import qualified Data.Map.Strict                      as Map
import           Data.List                            (find, maximumBy)
import           Data.Maybe                           (fromJust, isJust)
import           Data.Ord                             (comparing)
import           System.Random                        (StdGen)
import           Data.Random                          (sampleState)
import           Data.Random.Distribution.Categorical (weightedCategorical)

-- A game specfication, containing the state type, action type and player type.
class (Ord a, Ord p) => Spec s a p | s -> a, s -> p where
    actions :: s -> [(Double, a)]
    payouts :: s -> [(p, Double)]
    apply   :: a -> s -> s
    player  :: s -> p

-- A search node.
data (Ord a, Ord p) => Node a p = Node
    { children    :: Map.Map a (Node a p)
    , meanPayouts :: Map.Map p Double
    , playCount   :: Double
    }

-- Create a blank node.
empty :: (Ord a, Ord p) => Node a p
empty = Node
    { children    = Map.empty
    , meanPayouts = Map.empty
    , playCount   = 0.0
    }

-- Select a child node from a parent node.
child :: (Ord a, Ord p) => a -> Node a p -> Maybe (Node a p)
child action node = Map.lookup action (children node)

-- Returns the best action from a given node and state.
bestAction :: Spec s a p => Node a p -> s -> a
bestAction node state =
    fst . maximumBy (comparing snd) . Map.toList $
    Map.map (\child -> meanPayouts child Map.! player state) (children node)

-- Exported monte carlo function.
monteCarlo :: Spec s a p => StdGen -> s -> Node a p -> (StdGen, Node a p)
monteCarlo rand state node = fst (selection rand state node)

-- Run monte carlo simulation, returning the updated node, the updated
-- random generator, and the resulting payout of the simulation.
selection :: Spec s a p
              => StdGen -> s -> Node a p
              -> ((StdGen, Node a p), Map.Map p Double)
selection rand state node
    | null (actions state) = ((rand, addPayouts curPayouts node), curPayouts)
    | isJust unexpanded    = selectSim rand state (fromJust unexpanded) node
    | otherwise            = selectUCT rand state node
    where
        curPayouts = Map.fromList (payouts state)
        unexpanded = findUnexpanded state node

-- Recursively call select on a child of this tree based on UCT.
selectUCT :: (Spec s a p)
             => StdGen -> s -> Node a p
             -> ((StdGen, Node a p), Map.Map p Double)
selectUCT rand state node =
    ((newRand, backprop action newPayouts child node), newPayouts)
    where
        action = uct state node
        ((newRand, child), newPayouts) =
            selection rand (apply action state) (children node Map.! action)

-- Create a new node and simulate from there.
selectSim :: (Spec s a p)
             => StdGen -> s -> a -> Node a p
             -> ((StdGen, Node a p), Map.Map p Double)
selectSim rand state action node = ((newRand, newNode), newPayouts) where
    (newRand, newPayouts) = simulate rand (apply action state)
    newNode =
        backprop action newPayouts (singleton newPayouts) node

-- Simulates the game randomly from a starting state.
simulate :: Spec s a p => StdGen -> s -> (StdGen, Map.Map p Double)
simulate rand state
    | null (actions state) = (rand, Map.fromList (payouts state))
    | otherwise            = simulate newRand childState
    where
        childState = apply childAction state
        (childAction, newRand) =
            sampleState (weightedCategorical (actions state)) rand

-- Update a node with the payout and the updated child node.
backprop :: (Ord a, Ord p) =>
            a -> Map.Map p Double -> Node a p -> Node a p -> Node a p
backprop action payouts child node =
    (addPayouts payouts node)
    { children = Map.insert action child (children node) }

--
-- Utility Functions
--

-- Create a singleton node.
singleton :: (Ord a, Ord p) => Map.Map p Double -> Node a p
singleton payoutMap =
    Node { children = Map.empty, meanPayouts = payoutMap, playCount = 1.0 }

-- Looks for an unexpanded action to start simulation from.
-- If it returns Just a, start simulating from (a).
-- If it returns Nothing, continue selecting down using the uct function.
findUnexpanded :: Spec s a p => s -> Node a p -> Maybe a
findUnexpanded state node = find isUnexpanded (map snd (actions state)) where
    isUnexpanded action = Map.notMember action (children node)

-- Finds the best action under UCB1 to continue selection.
-- This function assumes that there are no unexpanded nodes or terminal nodes.
uct :: (Spec s a p) => s -> Node a p -> a
uct state node = maximumBy (comparing getScore) (map snd (actions state)) where
    getScore action = ucb (children node Map.! action)
    ucb childNode = (meanPayouts childNode Map.! player state) +
                    sqrt 2 * (sqrt (log (playCount node)) / playCount childNode)

-- Update a node with the payout.
addPayouts :: (Ord a, Ord p) => Map.Map p Double -> Node a p -> Node a p
addPayouts payouts node =
    node
    { meanPayouts =
          Map.unionWith (addToMean (playCount node)) payouts (meanPayouts node)
    , playCount = playCount node + 1
    }

-- Add a number to a mean, given we know how many numbers make up the mean.
addToMean :: Double -> Double -> Double -> Double
addToMean counts number mean = (mean * counts + number) / (counts + 1)
