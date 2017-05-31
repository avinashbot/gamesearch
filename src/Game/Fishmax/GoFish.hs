{-# LANGUAGE MultiParamTypeClasses #-}

module Game.Fishmax.GoFish where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Game.Fishmax.TreeSearch (Spec(..))

-- A Card can just be one of 12 cards, but using a string lets us adapt to
-- custom decks.
newtype Card = Card String deriving (Ord)

-- A game action
data Action = Ask Int Card deriving (Eq, Ord)

-- A player with some hidden information.
data Player = Player
    { cards :: Map.Map Card Int
    , books :: Set.Set Card
    }

-- A game state
data State = State
    { suits :: Set.Set Card
    , selfTurn :: Int
    , players :: [Player]
    , turn :: Int
    }

-- Implement the Spec typeclass.
instance Spec State Action where
    -- If the game is over and there's a winner.
    isFinal s = True

    -- Returns the number of books we put down.
    payout s = Just 0.0

    -- Returns cards they can ask.
    actions s = [Ask 0 (Card "J")]

    -- Applies Ask to State s.
    apply (Ask p c) s = s
