{-

DISCLAIMER:

I wish to make it clear that I worked with Juan Arroyo Miranda for this project.

We completed this project in 4 days since we were hammered with our PhD Machine Learning Midterm.
-}

module GameLogic (
    GameState(..),
    GameStatus(..),
    turnCom,
    turnHuman,
    isHandOrGameOver,
)where

import Deck
import Card
import Player
import Helper_Functions

{- A game status type that is used to signal to the GUI that current state of the game -}
data GameStatus = Active | GameOver | RoundOver
                deriving (Show, Eq)


{- A game state type keeps track of the deck and states of the players in the game  -}
data GameState  = GameState { deck    :: Deck,
                              players :: [PlayerState]}
                deriving (Show, Eq)

{- TESTING VARIABLES
-- comp1 = PlayerState {score=5, hand = Hand [Card Red 4, Card Yellow 7, Wild, Draw4, Draw2 Red], player = COM1}
-- comp2 = PlayerState {score=74, hand = Hand [Card Blue 4], player = COM2}
-- ibrahim = PlayerState {score=450, hand = Hand [Card Yellow 7], player = Human}
-- list_of_players = [ibrahim, comp1, comp2]
-- g = GameState newDeck list_of_players
-}
{-
   I know this description looks like a lot but I'm trying to be as detailed as possible.

   The "turnCom" function is called by the GUI for a computer's turn. Here's a description of each
   parameter for this function:

   =====
   gState:: GameState ==> The current state of the game. You will update this state at some point in this function
   and return a new GameState with those changes.

   player :: Player ==> This parameter specifies which computer turn it is (i.e. COM1 or COM2)

   (card, color) :: (Card, Maybe Color) ==> This parameter represents the current up card. The reason why it's a tuple is that
   the previous player may have placed a "wild" special card down as the up card. This means the previous player must have indicated a color.
   Thus, the second component is a Maybe value because it may have a Color specified if the card is a "wild" type of card or not.

    randomNum :: Integer ==> This parameter represents a random number that is either: 1,2,3, or 4. You may want to use this to determine the
                color for a wild card the computer may place down.
    ======

    Based on a the rules of the game , the current up card, and the strategy of the computer, you will need to update the GameState and maybe
    return a card for this computer's turn.

    "Updating the GameState" means that the computer may need to draw cards from the deck and place those cards in the computer's hand or remove
    cards from the computer's hand. Thus, you are modifying the GameState. Remember you need to adhere to the rules of Uno. Thus, you need to look at the up card first and adhere to
    the rules about the up card and then proceed with the strategy for the computer.

    So why is this function returning a tuple as such: (GameState, Maybe (Card, Maybe Color)) ?

    1st component: "GameState" ==> Represents the update game state after playing a computer's turn.
    2nd component: Maybe (Card, Maybe Color) ==> Based on the rules, the computer may not place down a card if it does not have
        a valid card (e.g., no cards in its hand matches the color or number of the up card). If the computer cannot put down a card then "Nothing" is returned.
        Otherwise, the computer returns the card it wants to put down. If that card is a "Wild" type of card then it needs to specify the color for the next turn.

        For example:
          Just (Wild, Red) ===> This means the computer is placing down a wild card with the color being Red
          Just (Card Red 3) ==> This means the computer is placing down a Red 3 card.
          Nothing ==> This means the computer has no card to place down.

-}
turnCom :: GameState -> Player -> (Card, Maybe Color) -> Int -> (GameState, Maybe (Card, Maybe Color))
turnCom  gState pname (card, color) randomNum
  | (condition1 && gOver1) || (condition1 && rOver1) = (updateScoresGameState any3gState, Just any3AndWild)
  | (condition1 && not gOver1) || (condition1 && not rOver1) = (any3gState, Just any3AndWild)
  | (condition2 && gOver2) || (condition2 && rOver2) = (updateScoresGameState sPany3gState, Just (any3AndSpecial, Nothing))
  | (condition2 && not gOver2) || (condition2 && not rOver2) = (sPany3gState, Just (any3AndSpecial, Nothing))
  | (condition3 && gOver3) || (condition3 && rOver3) = (updateScoresGameState highestgState, Just (highestmodal, Nothing))
  | (condition3 && not gOver3) || (condition3 && not rOver3) = (highestgState , Just (highestmodal, Nothing))
  | (condition4 && gOver4) || (condition4 && rOver4) = (updateScoresGameState lastmovegState, Just (lastmove, Nothing))
  | (condition4 && not gOver4) || (condition4 && not rOver4) = (lastmovegState, Just (lastmove, Nothing))
  | otherwise                                                = (notmatchedgState, Just (card, color))

  where player_hand = hand $ filter ( \ x -> player x == pname) (players gState) !! 0 --Returns Hand
        modal_color_hand = maxColors player_hand
        game_info = firstChunk gState pname (card, color) -- Returns 3-tuple
        update_input = secondChunk gState pname game_info -- Returns tuple with updated Deck and Hand
        updated_game = updateGameStateHandDeck gState pname update_input -- Updated GameState
        -- Case 1: Opponents have less than or equal to 3 Cards --> Play Wild
        condition1 = (anyHandLessThanThree game_info) && (anyWild player_hand)
        gOver1     = isHandOrGameOver any3gState == GameOver
        rOver1     = isHandOrGameOver any3gState == RoundOver
        any3AndWild = trueBranchThirdChunk updated_game pname -- Returns (Card, Maybe Color)
        any3gState = updateHandGameState (fst any3AndWild) updated_game pname
        -- Case 2: Opponents have <= 3 Cards, but I have no Wild or Draw4
        -- I play Special Cards (Revese, Split, Draw2) with same color as upcard
        condition2 = (anyHandLessThanThree game_info) && not(anyWild player_hand) && (anySpecial player_hand)
        any3AndSpecial = falseBranchChunk3 updated_game pname (card, color) -- Returns a Card
        sPany3gState = updateHandGameState any3AndSpecial updated_game pname
        gOver2 = isHandOrGameOver sPany3gState == GameOver
        rOver2 = isHandOrGameOver sPany3gState == RoundOver
        -- Case 3: Opponents have more than 3 Cards. I play the cards with my modal color.
        -- If modal color matches UPCARD color
        condition3 = not condition1 && not condition2 && (modal_color_hand == (getColor (card, color)))
        highestmodal = trueBranchFourthChunk updated_game pname (card, color) -- Returns a Card
        highestgState = updateHandGameState highestmodal updated_game pname
        gOver3 = isHandOrGameOver highestgState  == GameOver
        rOver3 = isHandOrGameOver highestgState  == RoundOver
        -- Case 4: Opponents have more than 3 cards. I try to find a card with the same
        -- number as the upcard in my modal color (Number matching).
        -- If I do not find one, I pick. Either way I check whether I'm allowed to play
        condition4 = (not condition3) && (allowedToPlay (card, color) lastmove)
        moveinfo = falseBranchFourthChunk updated_game pname (card, color)
        lastmove = fst moveinfo
        lastmovegState = updateHandGameState lastmove updated_game pname
        gOver4 = isHandOrGameOver lastmovegState == GameOver
        rOver4 = isHandOrGameOver lastmovegState == RoundOver
        -- Case 5: If condition 4 does not hold because allowedToPlay is False
        -- This implies I picked a card that does not match the upcard.
        -- In this case, I add the card to my deck, update the GameState and
        -- return the upcard. This should be the otherwise.
        notmatchedgState = snd moveinfo

{- Returns a tuple of the color of the upcard, False if the upcard is not a
draw card, and a tuple with opponent hand lengths
This function is central to our strategy. We assess that if opponents hand lengths
are less than or equal to 3, then we will play wild cards (if any) with a matching upcard color.
If no wilds are present, we will play a special card with a matching color if present in our hand-}
firstChunk :: GameState -> Player ->(Card, Maybe Color) ->(Color, (Card, Bool), (Integer, Integer))
firstChunk gState COM1 upcard = (upcolor, cardAndBool $ isItADrawCard upcard, (human_points, com2_points))
                                  where human_points = playerHandLength Human gState
                                        com2_points  = playerHandLength COM2 gState
                                        upcolor = getColor upcard

firstChunk gState COM2 upcard = (upcolor, cardAndBool $ isItADrawCard upcard, (human_points, com1_points))
                                  where human_points = playerHandLength Human gState
                                        com1_points  = playerHandLength COM1 gState
                                        upcolor = getColor upcard



{- Update COM1 and COM2 hands given the information about the Upcard
This function ensure thats the COM draws the appropriate number of cards if the
upcard is a Draw2 or a Draw 4
-}
secondChunk :: GameState -> Player -> (Color, (Card, Bool), (Integer, Integer)) -> (Deck, Hand)
secondChunk gState pl (upcolor, (upcard, boolean) , (n1 , n2))
 | (upcard == Draw4) && (boolean == True) = hand4
 | (upcard == Draw2 {color=upcolor}) && (boolean == True) = hand2
 | otherwise  = (deck gState, Hand [])
 where hand4 = applyNTimesNew 4 (snd.draw) (deck gState)
       hand2 = applyNTimesNew 2 (snd.draw) (deck gState)

{-
If anyWild returns True, then we call this function that returns the Wild Card.
We pick the color of the Wild based on the modal Color of our Cards
-}
trueBranchThirdChunk :: GameState -> Player -> (Card, Maybe Color)
trueBranchThirdChunk  gState pname  = (head $ getWilds myhand, Just mycolor) where
      myhand = hand $ filter ( \ x -> player x == pname) (players gState) !! 0
      mycolor = maxColors $ distinctFromWild myhand

{- This function assumes that we have a special card with the same color as the
upcard. This function will return that special card. This is only executed
if we do not have a wild card in our current hand or if opponents have more than
3 cards in their hands.-}
falseBranchChunk3 :: GameState -> Player -> (Card, Maybe Color) -> Card
falseBranchChunk3 gState pname (card, color) = [x | x <- mycards, x == Skip col || x == Reverse col || x == Draw2 col] !! 0
    where mycards = extractCardsFromHand $ hand $ [x | x <- players gState, player x == pname] !! 0
          col = getColor (card, color)

{- This function will play the hihgest rank card in our hand that matches the
color of the upcard. This is executed if we could not find a special card matching
the color of the upcard.-}
trueBranchFourthChunk :: GameState -> Player -> (Card, Maybe Color) -> Card
trueBranchFourthChunk gState pname (card, maycol) =
  highRankForColor upcard_color myhand where
   myhand = hand $ filter ( \ x -> player x == pname) (players gState) !! 0
   upcard_color = getColor (card, maycol)

{-This function takes the rank of the upcard and checks if we have the same rank in
ANY colour in our current hand. This function is only executed if we do not find a greater
than or equal to upcard rank in the current upcard color in our own hand.-}
falseBranchFourthChunk :: GameState -> Player -> (Card, Maybe Color) -> (Card, GameState)
falseBranchFourthChunk gState pname (upcard, maycol)
  | snd cardToPlay  =  (fst cardToPlay, gState)
  | otherwise       =  (snd drawonce, second)
  where myhand = hand $ filter ( \ x -> player x == pname) (players gState) !! 0
        cardToPlay = upCardMaxcolors (upcard, maycol) myhand
        drawonce = draw (deck gState)
        newhand = addToHand myhand (Hand [snd drawonce])
        myscore = score $ filter ( \ x -> player x == pname) (players gState) !! 0
        newstate = [PlayerState {score = myscore, hand = newhand, player = pname}]
        other_players = filter ( \ x -> player x /= pname) (players gState)
        c = getColor (upcard, maycol)
        second = if (upcard /= Draw4) && (upcard /= Draw2 {color= c}) then (GameState (fst drawonce) (other_players ++ newstate)) else gState

{-
Once the Player knows the type and color of the Upcard, this function updates
the GameState with the current Deck and Player's Hand.

This function assumes that the player has already picked a card from the deck.
As such, we need to add this card to the current hand and update the gamestate
before we execute our strategy. That is, is there now any card in our updated hand that
can be played for the given upcard.
-}
updateGameStateHandDeck :: GameState -> Player -> (Deck, Hand) -> GameState
updateGameStateHandDeck or_gState com (newdeck, extra_hand)
  | (deck or_gState) /= newdeck  = GameState newdeck (other_players ++ newstate)
  | otherwise                    = GameState (deck or_gState) (other_players ++ newstate)
  where other_players = filter ( \ x -> player x /= com) (players or_gState)
        myhand  = hand $ filter ( \ x -> player x == com) (players or_gState) !! 0
        myscore = score $ filter ( \ x -> player x == com) (players or_gState) !! 0
        newhand = addToHand myhand extra_hand
        newstate = [PlayerState {score = myscore, hand = newhand, player = com}]



{- This function REMOVES a card from a players hand after it is played
-}
updateHandGameState :: Card -> GameState -> Player -> GameState
updateHandGameState card gState pname = GameState (deck $ gState) (other_players++newstate)
    where other_players = filter ( \ x -> player x /= pname) (players gState)
          myhand  = removeCardFromHand card $ hand $ filter ( \ x -> player x == pname) (players gState) !! 0
          myscore = score $ filter ( \ x -> player x == pname) (players gState) !! 0
          newstate = [PlayerState {score = myscore, hand = myhand, player = pname}]

{- This function updates the scores for players after the gamestate indicates that
a game or round is over. We assess whether a game or round is over in the turnCom function.
-}
updateScoresGameState :: GameState -> GameState
updateScoresGameState gState = GameState (deck gState) (other_players ++ newstate)
   where
     winner_hand = hand $ filter ( \x -> (lenHand $ hand x) == 0) (players gState) !! 0
     winner = player $ filter ( \x -> (lenHand $ hand x) == 0) (players gState) !! 0
     winner_score = score $ filter ( \ x -> player x == winner) (players gState) !! 0
     other_players = filter ( \x -> (lenHand $ hand x) /= 0) (players gState)
     winner_points = sum $ map totalScore $ map hand other_players
     newstate = [PlayerState {score = winner_score + winner_points, hand = winner_hand, player = winner}]

{-  This functions uses the GameState parameter to determine the status of the game. The function should return a GameStatus
    based on the following:

    Active   ==> The current hand for the game is still going (i.e., every player has cards left)
    HandOver ==> The current hand is done because someone got a "uno" (i.e., a player has no cards left)
    GameOver ==> The game is over because a player has a score of 500 points or more.
-}

isHandOrGameOver :: GameState -> GameStatus
isHandOrGameOver gState
    | handOver gState = RoundOver
    | gameOver gState = GameOver
    | otherwise = Active

-- If any player has >= 500 points, the Game is over.
-- helper for isHandOrGameOver
gameOver :: GameState -> Bool
gameOver gstate = any (>= 500) $ map score $ players gstate

-- If any player has an empty hand, then the round is over.
-- helper for isHandOrGameOver
handOver :: GameState -> Bool
handOver gstate = any (==0) $ map lenHand $ map hand $ players gstate

-- calculated the length of a hand for a given player in a given gamestate.
-- helper for isHandOrGameOver
playerHandLength :: Player -> GameState -> Integer
playerHandLength name gState = lenHand $ map hand [x | x <- players gState, player x == name] !! 0

{-
   Since you (i.e., the student) will represent the human then you already know the rules of the game. Thus, "turnHuman" is function
   that acts as validity check. The function checks to make sure the card the human wants to place down is valid based on the game
   rules and the current up card. Here's a description of each parameter to the function:

   =====
   gState:: GameState ==> The current state of the game. You will update this state at some point in this function
   and return a new GameState with those changes.

   playerCard :: (Int, Maybe Color) ==> This parameter provides the index of card the human wants to place down in its Hand. If the
   card chosen by the human is a "wild" card then the user also provides a color.

   (card, color) :: (Card, Maybe Color) ==> This parameter represents the current up card. The reason why it's a tuple is that
   the previous player may have placed a "wild" special card down as the up card. This means the previous player must have indicated a color.
   Thus, the second component is a Maybe value because it may have a Color specified if the card is a "wild" type of card or not.
    ======


   IF the card the human wants to place down is not valid then the function should do the following:
        1. Deduct two points from the player's score
        2. Return a tuple with the modified game state and "False" as the second component to indicate the card was invalid
   ELSE the card is valid and the function should do the following:
        1. Remove the card from the player's hand
        2. Return a tuple with the modified game state and "True" as the second component to indicate the card was valid.

-}
turnHuman :: GameState ->  (Int, Maybe Color) -> (Card, Maybe Color) -> (GameState, Bool)
turnHuman gState playerCard upCard
    | condition1 || condition2 || condition3 = (GameState (deck $ gState) (other_players++newstate_True), True)
    | otherwise = (GameState (deck $ gState) (other_players++newstate_False), False)
    where cardToBePlayedTuple = constructCard gState playerCard
          cardToBePlayedActual = fst cardToBePlayedTuple
          upcardActual = fst upCard
          condition1 = (cardToBePlayedTuple == upCard)
          condition2 = ((getColor $ cardToBePlayedTuple) == getColor upCard)
          condition3 = cardRank upCard cardToBePlayedActual
          updatedhand = removeCardFromHand (fst $ cardToBePlayedTuple) (hand $ [x | x <- players gState, player x == Human] !! 0)
          myscore = score $ filter ( \ x -> player x == Human) (players gState) !! 0
          other_players = filter ( \ x -> player x /= Human) (players gState)
          newstate_True = [PlayerState {score = myscore, hand = updatedhand, player = Human}]
          modified_score = (score $ filter ( \ x -> player x == Human) (players gState) !! 0) - 2
          current_hand = hand $ [x | x <- players gState, player x == Human] !! 0
          newstate_False = [PlayerState {score = modified_score, hand = current_hand, player = Human}]

-- This function picks the card from a humans hand given an integer.
-- we assume that the integer will always be <= the length of the Hand.
constructCard :: GameState -> (Int, Maybe Color) -> (Card, Maybe Color)
constructCard gState (numeric, color) = ((extractCardsFromHand $ hand $ [x | x <- players gState, player x == Human] !! 0) !! numeric, color)
