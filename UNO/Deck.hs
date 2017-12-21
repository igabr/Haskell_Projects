module Deck (
    Deck(..),
    newDeck, 
    draw
) where 

import Card 

newtype Deck = Deck [Card]
    deriving (Show, Eq)  


{- 
   This variable returns a deck with all 108 cards for Uno. 

   See the image as an example: https://en.wikipedia.org/wiki/Uno_(card_game)#/media/File:UNO_cards_deck.svg

   You do not need to shuffle this deck. I will do that in my GUI code. 

-}
newDeck :: Deck 
newDeck = Deck (zeroToNine ++ oneToNine ++ (concat $ replicate 2 skipInit) ++ (concat $ replicate 2 reverseInit) ++ (concat $ replicate 2 draw2Init) ++ wildInit ++ draw4Init)

zeroToNine = [Card x y | x <- [Red, Green, Blue, Yellow], y <- [0..9]]

oneToNine = [Card x y | x <- [Red, Green, Blue, Yellow], y <- [1..9]]

skipInit = [Skip x | x <- [Red, Green, Blue, Yellow]]

reverseInit = [Reverse x | x <- [Red, Green, Blue, Yellow]]

draw2Init = [Draw2 x | x<- [Red, Green, Blue, Yellow]]

wildInit = replicate 4 Wild

draw4Init = replicate 4 Draw4

lenDeck :: Deck -> Int
lenDeck (Deck lst) = length lst

{- 
   This function takes the top most card from the deck and returns a new deck with that card removed 
   and the card
 -}
draw :: Deck -> (Deck, Card)
draw (Deck (x:xs)) = (Deck xs, x)

