module Helper_Functions
(
 lenHand,
 -- randColor,
 fromJust,
 getColor,
 anyWild,
 anySpecial,
 highRankForColor,
 allowedToPlay,
 scoreCard,
 totalScore,
 instances,
 countColors,
 extractMaxColor,
 maxColors,
 upCardMaxcolors,
 getWilds,
 addToHand,
 distinctFromWild,
 cardAndBool,
 isItADrawCard,
 extractCardsFromHand,
 removeCardFromHand,
 cardRank,
 anyHandLessThanThree,
 applyNTimesNew

)where

import Deck
import Card
import Player


-- #######################################################
--           Functions for manipulating individual Cards
--                or player's Hand
-- #######################################################

-- Checks if the card is a draw card
isItADrawCard :: (Card, Maybe Color) -> ((Card, Maybe Color), Bool)
isItADrawCard xs@(Draw2 _, _) = (xs, True)
isItADrawCard xs@(Draw4, _ ) = (xs, True)
isItADrawCard xs@(card, _ ) = (xs, False)

-- This function extracts all cards from a hand
extractCardsFromHand :: Hand -> [Card]
extractCardsFromHand (Hand lst) = lst

-- This function removes a card from a hand
removeCardFromHand :: Card -> Hand -> Hand
removeCardFromHand card (Hand lst) = Hand [x | x<- lst, x /= card]

-- Compares the rank of the Upcard and the Human Card to be played
cardRank :: (Card, Maybe Color) -> Card -> Bool
cardRank (card, col) toplay =
    case card of
      Wild      -> False
      Draw4     -> False
      Skip _    -> False
      Reverse _ -> False
      Draw2 _   -> False
      Card _ _  -> rank card == rank toplay

-- Checks whether any one of the opponents has less than 3 Cards in their Hand
-- used by the computer for intelligent strategy
anyHandLessThanThree :: (Color, (Card, Bool), (Integer, Integer)) -> Bool
anyHandLessThanThree (col, (card, bool), (x,y)) = any (<=3) (x,y)

-- Computes the length of a Hand
lenHand :: Hand -> Integer
lenHand (Hand lst) = foldr (\_ n -> 1 + n) 0 lst

-- checks if a given color is present in a given hand
isColourInHand :: Color -> Hand -> Bool
isColourInHand col (Hand lst) = (elem) col (map color lst)

-- extracts just the card and the bool from isItADrawCard -- used in firstchunk
cardAndBool :: ((a, b1), b2) -> (a, b2)
cardAndBool ((a, b), c) = (a,c)

-- Get value from Maybe constructor
fromJust :: Maybe a -> a
fromJust (Just a) = a
fromJust Nothing = error "Am I Nothing?"

-- returns the color of a card
getColor :: (Card, Maybe Color) -> Color
getColor(card, may)
 | (card == Wild) || (card == Draw4) = fromJust may
 | otherwise                         = color card


-- Checks whether Hand has any cards different from Wild and Draw4
distinctFromWild :: Hand -> Hand
distinctFromWild (Hand lst) = Hand (filter (\ x-> (x /= Wild) && (x/= Draw4)) lst)  

-- Check if I have any Wild or Draw4 cards in Hand
anyWild :: Hand -> Bool
anyWild (Hand lst)
 | (Wild `elem` lst) || (Draw4 `elem` lst) = True
 | otherwise                               = False

-- Check if I have any Special (Draw2, Reverse, Split) card in Hand
anySpecial :: Hand -> Bool
anySpecial (Hand lst) =
  any (==True) $ map checkSpecial lst where
    checkSpecial Draw2 {color = _} = True
    checkSpecial Skip {color = _}  = True
    checkSpecial Reverse {color = _} = True
    checkSpecial _  = False


-- Checks whether Hand has any cards different from Wild and Draw4
-- same as distinctFromWild but return [Card] and NOT a Hand
getWilds :: Hand -> [Card]
getWilds (Hand lst)
 | anyWild (Hand lst)  = filter (\ x-> (x == Wild) || (x== Draw4)) lst
 | otherwise           = []

-- #######################################################
--           Functions for Implementing AI Strategy
--             Main functions: maxColors, allowedToPlay,
--                      highRankForColor
-- #######################################################


-- counts the instances of a color in a list -- helper for maxColors
instances::Color->[Color]->Int
instances _ [] = 0
instances x (y:ys)
    | x == y = 1 + (instances x ys)
    | otherwise = instances x ys

-- creates a list of the colors and their occurrences in a hand -- helper for maxColors
countColors :: [Color] -> [(Color, Int)]
countColors lst = let red_val = instances Red lst
                      yellow_val = instances Yellow lst
                      blue_val = instances Blue lst
                      green_val = instances Green lst
              in [(Red, red_val), (Green, green_val), (Yellow, yellow_val), (Blue, blue_val)]

-- returns the tuple with the highest instance count -- helper for maxColors
extractMaxColor :: [(Color, Int)] -> (Color, Int)
extractMaxColor []     = error "maximum of empty list"
extractMaxColor (x:xs) = maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail (m, n) (p:ps)
          | n < (snd p) = maxTail p ps
          | otherwise   = maxTail (m, n) ps

-- Returns the mode color in a given hand.
-- this function is central to our intelligent strategy
maxColors :: Hand -> Color
maxColors (Hand lst) = fst $ extractMaxColor $ countColors $ map color lst

-- Helper function for any Special card in upCardMaxcolors -- helper for upCardMaxcolors
maxSpecialCards :: (Card, Maybe Color) -> Hand -> (Card, Bool)
maxSpecialCards (upcard, maycol) (Hand lst)
 | isColourInHand (getColor (upcard, maycol)) (Hand lst) == False  = (upcard, False)
 | otherwise     =  (mycard , True)
 where mycard = highRankForColor (getColor (upcard, maycol)) (Hand lst)

-- Helper function for cards 0 to 9 for upCardMaxcolors -- helper for upCardMaxcolors
maxNormalCards :: (Card, Maybe Color) -> Hand -> (Card, Bool)
maxNormalCards (upcard, maycol) (Hand lst)
  | length filtered > 0  = (head filtered, True)
  | otherwise           = (upcard, False)
  where upcard_value = rank upcard
        mymaxcolor =  maxColors (Hand lst)
        filtered  = filter (\ x -> color x == mymaxcolor && rank x == upcard_value) lst

{- Checks whether we have a card with the same value as the Upcard in the set
of cards with our most frequent color in our Hand
-}
upCardMaxcolors :: (Card, Maybe Color) -> Hand -> (Card, Bool)
upCardMaxcolors (upcard, maycol) (Hand lst) =
  case upcard of
    Wild      -> maxSpecialCards (upcard, maycol) (Hand lst)
    Draw4     -> maxSpecialCards (upcard, maycol) (Hand lst)
    Draw2 _   -> maxSpecialCards (upcard, maycol) (Hand lst)
    Reverse _ -> maxSpecialCards (upcard, maycol) (Hand lst)
    Skip _    -> maxSpecialCards (upcard, maycol) (Hand lst)
    Card _ _  -> maxNormalCards  (upcard, maycol) (Hand lst)


-- Get card with highest rank in Hand given a color
highRankForColor :: Color -> Hand -> Card
highRankForColor col (Hand lst) = Card {color = col, rank = maxnumber} where
  filtered = filter (\ x -> color x == col) lst
  maxnumber = maximum $ map rank filtered

-- Checks whether Player is allowed to Play a Numbered Card given an Upcard
allowedNumbered :: (Card, Maybe Color) -> Card -> Bool
allowedNumbered (card1, maycol) card2
 | (color card1 == color card2) || (rank card1 == rank card2) = True
 | otherwise                               = False

-- Checks whether Player is allowed to Play Special Card given an Upcard
allowedSpecial :: (Card, Maybe Color) -> Card -> Bool
allowedSpecial (card1, maycol) card2
 | (color card1 == color card2)  = True
 | otherwise                     = False

-- Pattern Matching for all card types
-- checks whether we are allowed to play a particular card for a given upcard
allowedToPlay :: (Card, Maybe Color) -> Card -> Bool
allowedToPlay (card1, maycol) card2 =
  case card1 of
    Wild      -> (getColor (card1, maycol) == color card2)
    Draw4     -> (getColor (card1, maycol) == color card2)
    Skip _    -> allowedSpecial (card1, maycol) card2
    Reverse _ -> allowedSpecial (card1, maycol) card2
    Draw2 _   -> allowedSpecial (card1, maycol) card2
    Card _ _  -> allowedNumbered (card1, maycol) card2

-- #######################################################
--           Functions for Updating GameState
--             Main functions: applyNTimesNew,
--                addToHand, totalScore
-- #######################################################

-- Call draw function n times, where f is a composite function snd.draw
-- essential function for when we see a Draw2 or Draw4 card
-- we think this function is a stroke of pure unrefined genius
applyNTimesNew:: Int -> (Deck -> Card) -> Deck -> (Deck, Hand)
applyNTimesNew n f deck = (newdeck, cards)
  where newdeck = fst input
        cards   = Hand (snd input)
        input   = foldl ( \ (param, accum) x -> (fst $ draw param, x param:accum)) (deck, []) (replicate n f)

-- Add Cards to Hand
addToHand :: Hand -> Hand -> Hand
addToHand (Hand new) (Hand lst) = Hand (new ++ lst)

-- Takes a card an returns integer val. Helper function for totalScore
scoreCard :: Card -> Integer
scoreCard Wild = 50
scoreCard Draw4 = 50
scoreCard Draw2 {color = c} = 20
scoreCard Skip {color = c} = 20
scoreCard Reverse {color = c} = 20
scoreCard card = rank card

-- Takes a Hand and returns the sum of the scores
totalScore :: Hand -> Integer
totalScore (Hand lst) = sum $ map scoreCard lst

-- -- Maps random number to color
-- -- we didnt end up using this function for the randomnumber that is passed
-- -- to the turnCom function.
-- randColor :: Integer -> Color
-- randColor 1 = Red
-- randColor 2 = Blue
-- randColor 3 = Yellow
-- randColor 4 = Green
-- randColor _ = error "This is not a valid number."
