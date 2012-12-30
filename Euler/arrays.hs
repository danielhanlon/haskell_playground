import Data.Array

data Rank = Two | Three | Four | Five | Six | Seven | Eight
          | Nine | Ten | Jack | Queen | King | Ace
          deriving (Eq, Ord, Show, Read, Enum, Ix)

data Suit = Clubs | Diamonds | Hearts | Spades
            deriving (Eq, Ord, Show, Read, Enum, Ix)

data GenCard = GenCard Rank Suit
               deriving (Eq, Ord, Show, Read)
