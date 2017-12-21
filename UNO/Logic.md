## Strategy

Both COM1 and COM2 will be playing the same _intelligent_ strategy.

### Overview of Strategy

- Step 1: Assess the Color and Type of the Upcard

In this step, the computer extracts the color and the type of the Upcard. That is, whether it is a Card, Wild, Draw2, Draw4 etc.

- Step 2: Calculate the number of cards in an opponents hands.

In this step, the computer calculates the length of the opponents hands via the GameState.

- Step 3: Are any of the opponent hand lengths <= 3

This is essential as our strategy dictates the following:

If any hand length is <= 3, then assess if you have a wild card in your own deck. If you do, then play that Wild card with the color that matched the **modal** color in our current hand. This is done using the maxColors function. This is an incredibly important function for our strategy.

The motivation behind this is that, if the opponents have <= 3 cards, then they might be close to winning a round. As such, we wish to minimize the possible points they could get. Since wild cards are worth 50 points, we wish to rid our hand of wild cards. Furthermore, by playing a wild with our modal color, we increase the likelihood that we will be able to later on drop more cards.

If none of the opponents hands are <= 3, then we wish to assess whether we have any special cards in our current deck that match the color of the up-card. The motivation here is that special cards are worth 20 points. Again, we are minimizing potential points the opponents could earn by winning a round later on in addition to reducing the number of cards in our hand.

- Step 4:

_What if we don't have any special cards that match the color of the up-card?_

In this case, we then assess what the **modal** color in our deck is and check to see if that matches the color of the up-card.

If it does, we then play the **highest ranked** card we have that belongs to the **modal** color set of cards in our hand. Again, this would be identical to the color of the up-card. In this situation, we would have changed the course of the game in that by playing a card with identical rank to the up-card and with a color that matches our modal color, we have opened the possibility of playing more cards with our modal color later in the game. This maximizes the chances of reducing the number of cards in our hand.

_However, lets say that the **modal** color in our hand **does not** match the color of the up-card?_

In this case, we look at the up-card and extract its color. We then search our hand to see if have any Wild cards that could be played.

If so, we will play a wild card with our **modal** color. It is **important to note** here that we are checking for possible wild cards at this stage, since it is possible to reach this point in the game logic without having ever have checked for wild cards.

If there are now wild cards to be played, we then extract the rank of the up-card and search the set of cards that are in out modal color and see if we have any that match the rank. Re-call, a card can be played if the colors match **OR** if the rank matches.

If we find a card within our modal color that matches the rank of the up-card, it will be played. Notice here that this step would be very beneficial to us in that, we got to play a card (and reduce our hand by 1) but we have also changed the state of the game to match the color of the cards we have the most of. This may prove beneficial later on in the game. by allowing us to place down more cards.

_However, what about the case where we do not have a card that matches the rank of the up-card?_

In this case, we are forced to pick a card from the deck.

We look at the card we have just picked. If it matches the color or the rank of the up-card, it is played. Otherwise, it is added to our hand and the gamestate is updated. If the new card cannot be played, then the turn is over.


**What if the up-card is a Draw2 or a Draw4?**

In this case, we first pull the appropriate number of cards from the deck, add them to our hand, update the gamestate and then see if we can play a card according to our strategy that is outlined above.

In order to draw 2 or draw 4 cards, we wrote a function called `applyNTimesNew`.

In our code, we allude to the 4 above steps as chunks. We also viewed our _intelligent strategy_ like one really big tree structure. As such, part of our code are called `falseBranchFourthChunk`. to refer to the step and the branch within that step.
