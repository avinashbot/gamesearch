{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Fishmax.PIGoFish where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Game.Fishmax.TreeSearch (Spec(..), Action)

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
    { selfTurn :: Int
    , players :: [Player]
    , pile :: [Card]
    , turn :: Int
    }

-- Implement the Spec typeclass.
instance Spec State Ask where
    -- Returns the number of books we put down.
    payout s = Just 0.0

    -- Returns cards and players they can ask.
    actions s = [Ask 0 (Card "J")]

    -- Applies Ask to State s.
    apply (Ask p c) s = s


count :: Player -> Card -> Int
count player card = Map.findWithDefault 0 card (cardCount player)

heldCards :: Player -> [Card]
heldCards player = Map.keys $ Map.filter (> 0) (cardCount player)
