module Game.Fishmax.GoFish where

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import System.Random (StdGen)

-- | The type of card (e.g. 1, 8, J)
newtype Card =
    Card Int
    deriving (Eq, Ord)

-- | A player with public and hidden information.
data Player = Player
      -- | The minimum number of cards publicly shared with the players. If the
      -- sum of the map is equal to the size of their hand, then the player is
      -- essentially transparent (or has been determinized).
    { cards :: Map.Map Card Int
      -- | Cards that were shared to the public, i.e. cards we know for sure.
      -- This set is used for opponent modelling.
    , shared :: Set.Set Card
      -- | The set of cards that the player definitely does not have. This is
      -- however, reset if the player picks up a card from the pile that could
      -- be the card.
    , excluded :: Set.Set Card
      -- | The number of cards the player is holding.
    , handSize :: Int
    }

-- | Returns the player's placed books.
books :: Int -> Player -> [Card]
books size player = Map.keys $ Map.filter (== size) (cards player)

-- | Returns the number of cards of that type a player has.
cardCount :: Card -> Player -> Int
cardCount card player = Map.findWithDefault 0 card (cards player)

-- | Give a certain number of cards to the player publicly.
receiveCards :: Int -> Card -> Player -> Player
receiveCards count card player =
    player
    { cards = Map.insert card (cardCount card player + count) (cards player)
    , shared = Set.insert card (shared player)
    , excluded = Set.delete card (excluded player)
    , handSize = handSize player + count
    }

-- | Remove a certain number of cards from the player publicly.
removeCards :: Int -> Card -> Player -> Player
removeCards count card player =
    player
    { cards = Map.insert card 0 (cards player)
    , shared = Set.delete card (shared player)
    , excluded = Set.insert card (excluded player)
    , handSize = handSize player - count
    }

-- | Add a card to the player's shared information.
shareInclusion :: Card -> Player -> Player
shareInclusion card player =
    player
    { cards =
          if cardCount card player == 0
              then Map.insert card 1 (cards player)
              else cards player
    , shared = Set.insert card (shared player)
    , excluded = Set.delete card (excluded player)
    }

-- | Store that the player does not have the card.
shareExclusion :: Card -> Player -> Player
shareExclusion card player =
    player
    { shared = Set.delete card (shared player)
    , excluded = Set.insert card (excluded player)
    }

-- | The details of the specific game being played.
data Game = Game
      -- | A simple sequence of players. A sequence was used since it
      -- allows for pretty straightforward mid-list updates.
    { players :: Seq.Seq Player
      -- | The type of cards
    , cardTypes :: [Card]
      -- | The size of a completed book.
    , bookSize :: Int
    -- | The index of the player to maximize the payouts for.
    , selfTurn :: Int
    -- | Keeps track of whose turn it is.
    , turn :: Int
    -- | A randomness generator for determinizing unknowns.
    , determinizer :: StdGen
    }

-- | Returns a histogram of publicly known cards.
knownCards :: Game -> Map.Map Card Int -> Map.Map Card Int
knownCards game accum = Map.unionWith (-) accum known
  where
    known =
        foldl (\s p -> Map.unionWith (+) s (cards p)) Map.empty (players game)

-- | Remove all keys from a map that are part of the player's exclusion set.
filterExcluded :: Player -> Map.Map Card Int -> Map.Map Card Int
filterExcluded player =
    Map.filterWithKey (\k _ -> Set.notMember k (excluded player))

-- | Decrements all cards that would have caused a book.
adjustCompletable :: Game -> Player -> Map.Map Card Int -> Map.Map Card Int
adjustCompletable game player unknowns =
    Map.mapWithKey
        (\card unknownCount ->
             if cardCount card player + unknownCount == bookSize game
                 then unknownCount - 1
                 else unknownCount)
        validUnknowns
  where
    validUnknowns = Map.filter (> 0) unknowns

-- | Returns a histogram of possible unknown cards for the player.
possibleCards :: Game -> Player -> Map.Map Card Int
possibleCards game player =
    adjustCompletable game player $
    filterExcluded player $
    knownCards game $
    Map.fromList [(c, bookSize game) | c <- cardTypes game]
