{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Game.Fishmax.TreeSearch
    ( Spec(..)
    , Action
    , Node

    , emptyNode
    , monteCarlo
    , payouts
    ) where

import           Data.List       (find, sortOn)
import           Data.Maybe      (fromJust, isJust)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as Set
import           System.Random   (RandomGen, randomR)

-- A game action.
class (Eq a, Ord a) => Action a

-- A game state.
class (Action a) => Spec s a | s -> a where
    -- Payout function for the player for the leaf state.
    payout  :: s -> Maybe Double
    -- Returns a list of legal actions for a given state.
    -- Returns [] if the node is a leaf node.
    actions :: s -> [a]
    -- Returns the result of applying an action to a state.
    apply   :: a -> s -> s

-- A node in the search tree.
-- In ISMCTS, there's really no such thing as a terminal node, because a node
-- that is terminal in one determinization can be non-terminal in another.
data Action a => Node a = Node
    { children   :: Map.Map a (Node a)
    , meanPayout :: Double
    , playCount  :: Double
    } deriving (Show)

-- Create a blank node.
emptyNode :: Action a => Node a
emptyNode = Node { children = Map.empty, meanPayout = 0, playCount = 0 }

-- Run monte carlo simulation, returning the updated node, the updated
-- random generator, and the resulting payout of the simulation.
monteCarlo :: (RandomGen r, Action a, Spec s a) =>
                  r -> s -> Node a -> (Node a, (r, Double))
monteCarlo rand state node
    | null (actions state) = (node, (rand, fromJust (payout state)))
    | isJust unexpanded    = selectSim (fromJust unexpanded) rand state node
    | otherwise            = selectUCT rand state node
    where unexpanded = findUnexpanded state node

-- Return a map of payouts for each initial action.
payouts :: Action a => Node a -> Map.Map a Double
payouts node = Map.map meanPayout (children node)

-- Looks for an unexpanded action to start simulation from.
-- If it returns Just a, start simulating from (a).
-- If it returns Nothing, continue selecting down using the uct function.
findUnexpanded :: (Action a, Spec s a) => s -> Node a -> Maybe a
findUnexpanded s n = find isUnexpanded (actions s) where
    isUnexpanded a = Map.notMember a (children n)

-- Create a new node and simulate from there.
selectSim :: (RandomGen r, Action a, Spec s a) =>
                 a -> r -> s -> Node a -> (Node a, (r, Double))
selectSim action rand state node =
    (backprop action (singletonNode payout) payout node, (newRand, payout))
    where
        (newRand, payout) = simulate rand (apply action state)

-- Recursively call select on a child of this tree based on UCT.
selectUCT :: (RandomGen r, Action a, Spec s a) =>
                 r -> s -> Node a -> (Node a, (r, Double))
selectUCT rand state node =
    (backprop action child payout node, (newRand, payout))
    where
        action = uct state node
        (child, (newRand, payout)) =
            monteCarlo rand (apply action state) (children node Map.! action)

-- Simulates the game randomly from a starting state.
simulate :: (RandomGen r, Spec s a) => r -> s -> (r, Double)
simulate rand state
    | null (actions state) = (rand, fromJust (payout state))
    | otherwise            = simulate newRand childState
    where
        (childIndex, newRand) = randomR (0, length stateActions - 1) rand
        childState = apply (stateActions !! childIndex) state
        stateActions = actions state

-- Create a singleton node.
singletonNode :: Action a => Double -> Node a
singletonNode payout =
    Node { children = Map.empty, meanPayout = payout, playCount = 1 }

-- Finds the best action under UCB1 to continue selection.
-- This function assumes that there are no unexpanded nodes or terminal nodes.
uct :: (Action a, Spec s a) => s -> Node a -> a
uct s n = fst $ fMax getScore (actions s) where
    getScore a = ucb (children n Map.! a)
    ucb c = meanPayout c + sqrt 2 * (sqrt (log (playCount n)) / playCount c)

-- fMax replaces `sortOn` in uct with a O(n) maximum function.
fMax :: Ord b => (a -> b) -> [a] -> (a, b)
fMax _ []  = error "cannot find max of empty list"
fMax f [x] = (x, f x)
fMax f (x:ys) = let (val, comp) = fMax f ys in
                if f x > comp then (x, f x) else (val, comp)

-- Update a node with the payout and the updated child node.
backprop :: Action a => a -> Node a -> Double -> Node a -> Node a
backprop action child payout node =
    Node
    { children = Map.insert action child (children node)
    , meanPayout = (meanPayout node * playCount node + payout) /
                   (playCount node + 1)
    , playCount = playCount node + 1
    }
