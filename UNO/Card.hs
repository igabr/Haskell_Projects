module Card 
(
    Card(..),
    Color(..), 
    Hand(..)
) where 

data Color = Red | Green | Blue | Yellow
    deriving (Show, Eq)     

newtype Hand = Hand [Card]
    deriving (Show,Eq)

data Card  = Card {color :: Color, rank::Integer}
           | Skip {color :: Color} 
           | Reverse {color :: Color}
           | Draw2 {color :: Color}
           | Wild 
           | Draw4 
    deriving (Show,Eq)