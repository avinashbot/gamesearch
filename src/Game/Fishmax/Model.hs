module Game.Fishmax.Model
    ( Card(..)
    , Player(..)
    ) where

import qualified Data.Map.Strict         as Map
import qualified Data.Set                as Set
import           Game.Fishmax.TreeSearch (Spec (..))

-- A Card can just be one of 12 cards, but using a string lets us adapt to
-- custom decks.
newtype Card = Card String deriving (Show, Eq, Ord)

-- A player with some hidden information.
data Player = Player
    { excluded  :: Set.Set Card
    , completed :: Set.Set Card
    , minCounts :: Map.Map Card Int
    , handSize  :: Int
    }

-- A game state
data State = State
    { suits    :: Set.Set Card
    , selfTurn :: Int
    , players  :: [Player]
    , turn     :: Int
    }

data Ask = Ask Int Card deriving (Eq, Ord)


--
-- Internal Methods
--


-- Give the player certain number of a given card.
receiveCards :: Int -> Card -> Player -> Player
receiveCards count card player =
    player { excluded  = Set.delete card (excluded player)
           , minCounts = Map.adjust (+ count) card (minCounts player)
           , handSize  = handSize player + count
           }

-- Remove a certain number of that card from the player's hand, assuming that's
-- all they have.
removeAllCards :: Int -> Card -> Player -> Player
removeAllCards count card player =
    player { excluded  = Set.insert card (excluded player)
           , minCounts = Map.insert card 0 (minCounts player)
           , handSize  = handSize player - count
           }

-- Ensure we are tracking that the player has at least one of this card.
declareInclusion :: Card -> Player -> Player
declareInclusion card player =
    if cardCount > 0 then player else addCard where
        cardCount = Map.findWithDefault 0 card (minCounts player)
        addCard = player { excluded  = Set.delete card (excluded player)
                         , minCounts = Map.insert card 1 (minCounts player)
                         }

-- Ensure we are tracking that the player does not have any of these cards.
declareExclusion :: Card -> Player -> Player
declareExclusion card player =
    player { excluded  = Set.insert card (excluded player)
           , minCounts = Map.insert card 0 (minCounts player)
           }

-- Add an unspecified card to the player's hand.
pickupCard :: Player -> Player
pickupCard player =
    player { excluded = Set.empty, handSize = handSize player + 1 }


--
-- Game Actions
--


-- Make a "correct" play, i.e. the asker guesses the player's cards correctly.
playCorrect :: Int -> Card -> (Player, Player) -> (Player, Player)
playCorrect count card (asker, giver) =
    ( receiveCards count card (declareInclusion card asker)
    , removeAllCards count card giver )

-- Make a "go fish" play, i.e. the asker has to pick up a card from the pile.
playGoFish :: Card -> (Player, Player) -> (Player, Player)
playGoFish card (asker, sayer) =
    (pickupCard (declareInclusion card asker), declareExclusion card sayer)

-- Make a "go fish" play where we know which card the player picked up from the
-- pile.
playKnownGoFish :: Card -> Card -> (Player, Player) -> (Player, Player)
playKnownGoFish takenCard askedCard (asker, sayer) =
    ( receiveCards 1 takenCard (declareInclusion askedCard asker)
    , declareExclusion askedCard sayer )

-- Place a group of one of the player's cards down.
-- In most games, the size is just 4.
placeBook :: Int -> Card -> Player -> Player
placeBook size card player =
    removedCards { completed = Set.insert card (completed player) } where
        removedCards = removeAllCards size card player


--
-- Heuristic Calculation Helpers
--


-- Makes completed cards almost completed (since completed is an invalid state)
reduceCompletable :: Int -> Map.Map Card Int -> Map.Map Card Int
reduceCompletable val = Map.map (\x -> if x == val then x - 1 else x)

-- Zeroes cards that we know the player doesn't have.
zeroExcluded :: Player -> Map.Map Card Int -> Map.Map Card Int
zeroExcluded p s = foldl (\m c -> Map.insert c 0 m) s (excluded p)

-- Zeroes cards based on the cards in books placed by all the players.
zeroCompleted :: [Player] -> Map.Map Card Int -> Map.Map Card Int
zeroCompleted p s = foldl (\m c -> Map.insert c 0 m) s completedAll where
    completedAll = foldl (\s x -> Set.union s (completed x)) Set.empty p

-- Reduces cards based on known minValues for all players.
reduceKnown :: [Player] -> Map.Map Card Int -> Map.Map Card Int
reduceKnown p s = foldl (\m x -> Map.unionWith (-) m (minCounts x)) s p

-- Returns a histogram of UNKNOWN cards that the player may have in their hand
-- based on the player's and their opponents' known cards.
possibleUnknowns :: Int -> [Card] -> Player -> [Player]
                        -> Map.Map Card Int
possibleUnknowns count cards self players =
    zeroExcluded self $
    zeroCompleted players $
    reduceCompletable count $
    reduceKnown players $
    Map.fromList [(c, count) | c <- cards]

-- Returns the number of cards that we don't know in the player's hand.
unknownCardCount :: Player -> Int
unknownCardCount player = handSize player - sum (minCounts player)

-- Gets the number of remaining players.
remainingPlayers :: [Player] -> Int
remainingPlayers ps = length $ filter (\p -> handSize p == 0) ps
