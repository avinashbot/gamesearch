{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Fishmax.PIGoFish where

import qualified Data.Map.Strict         as Map
import qualified Data.Set                as Set
import           Data.Array.IArray       (Array, array, bounds, elems, (!), (//))
import           Game.Fishmax.TreeSearch (Spec(..), Action)

-- A Card can just be one of 12 cards, but using a string lets us adapt to
-- custom decks.
newtype Card = Card String deriving (Eq, Ord)

-- A game action
data Ask = Ask Int Card deriving (Eq, Ord)
instance Action Ask

-- A player with some hidden information.
data Player = Player
    { cardCount :: Map.Map Card Int
    , books :: Set.Set Card
    }

-- A game state
data State = State
    { selfTurn    :: Int
    , players     :: Array Int Player
    , pile        :: [Card]
    , turn        :: Int
    }

-- Implement the Spec typeclass.
instance Spec State Ask where
    -- Returns cards and players they can ask.
    actions s
        | all (\p -> sum (cardCount p) == 0) (players s) = []
        | otherwise = possibleAsks s

    -- Returns the number of books we put down.
    payout s
        | null (actions s) = Nothing
        | otherwise = Just (fromIntegral (Set.size (books (players s ! selfTurn s))))

    -- Applies Ask to State s.
    apply (Ask p c) s =
        if corrCards > 0 then applyCorrect p c s else applyGoFish c s
        where
            corrCards = Map.findWithDefault 0 c (cardCount (players s ! p))

-- Lists the current player's actions.
possibleAsks :: State -> [Ask]
possibleAsks s = [Ask i c | i <- [0..maxIndex], c <- uniqCards]
    where
        (_, maxIndex) = bounds (players s)
        player = players s ! turn s
        uniqCards = Map.keys $ Map.filter (> 0) (cardCount player)

-- Update books
updateBooks :: Player -> Player
updateBooks p = p
    { cardCount = remaining
    , books = Set.union (books p) (Set.fromList (Map.keys toAdd))
    } where
    (toAdd, remaining) = Map.partition (== 4) (cardCount p)

-- Give the player certain number of a given card.
receiveCards :: Int -> Card -> Player -> Player
receiveCards count card player = updateBooks
    (player { cardCount = Map.insert card (current + count) (cardCount player) })
    where
        current = Map.findWithDefault 0 card (cardCount player)

-- Remove a certain number of that card from the player's hand, assuming that's
-- all they have.
removeAllCards :: Card -> Player -> Player
removeAllCards card player =
    player { cardCount = Map.insert card 0 (cardCount player) }

-- Run a scenario where the asker guesses correctly.
applyCorrect :: Int -> Card -> State -> State
applyCorrect p c s = s
    { players = players s // [(p, removeAllCards c giver),
                              (turn s, receiveCards transferred c asker)]
    } where
        giver = players s ! p
        asker = players s ! turn s
        transferred = Map.findWithDefault 0 c (cardCount giver)

-- Run a scenario where the asker guesses incorrectly.
applyGoFish :: Card -> State -> State
applyGoFish c s = s
    { turn = (turn s + 1) `mod` (maxIndex + 1)
    , pile = if null (pile s) then [] else tail (pile s)
    , players = players s // [(turn s, receiveCards 1 (head (pile s)) asker)]
    } where
        asker = players s ! turn s
        (_, maxIndex) = bounds (players s)
