module Player (
    Player(..),
    PlayerState(..)
)where 

import Card 

data Player = Human | COM1 | COM2 
    deriving (Show, Eq)  

data PlayerState = PlayerState { score  :: Integer, 
                                 hand   :: Hand, 
                                 player :: Player}
    deriving (Show, Eq)