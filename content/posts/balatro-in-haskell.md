---
date: '2025-06-05T10:54:00+10:00'
draft: true
title: 'Balatro Scoring in Haskell'
tags:
- haskell
- programming
publishDate: '2025-06-13'
---
Recently, I've been obsessed with the game Balatro, a deckbuilding roguelike where you play poker hands to generate points and beat the required score for each round. Points are calculated by multiplying together two numbers: Chips and Mult (as in multiplier), which can both be increased by your build and the poker hands you play.

The gameplay loop looks like:

1. Play rounds of playing cards to generate enough points to beat the required score and move onto the next 
2. Enhance your build in the shop by buying:

    * Jokers, which provide permanent buffs while they remain in your build
    * Planets, consumable cards which increase the base Chips and Mult of a poker hand
    * Tarots, consumable cards which generate money and enhance your Playing Cards and Jokers
    * Spectrals, consumable cards which provide powerful buffs, but with serious drawbacks

In other words, I like it when cards go BRRRRRRRR and number goes up! I work at the intersection of finance and tech, of course 90% of my favourite video games are basically spreadsheets with fancy graphics (CK3, EU4, Factorio, etc.)

I'm not going to fully recreate Balatro, obviously, but I'd like to use the score calculation mechanics to show how functional programming lets you encode a domain in software. I'm constantly asking myself "what do I mean?" when I write software, because I'm trying to embed the semantics of a domain into the code, not just the menial details that the computer needs to perform the behaviour I want.

The most important consumer of source code isn't the computer—if the output behaviour is correct, it doesn't really matter to the computer (barring business concerns like memory usage, performance, and so on) how the code is written.

I submit that the most important consumers of the source code are the developers. The closer the source code hews to the human understanding of the domain, the easier it is for developers to have a conversation with domain specialists about the semantics, the meaning, of what they're trying to translate into software.

If you like fancy buzzwords, you can look into "domain-driven design" for some more ideas about this.

So, with that spiel over and done with, let's get our teeth into writing some code.

## Poker

Let's start with some basic types for the domain of poker, and then we'll enhance our code to deal with Balatro scoring later. We're going with the French cards (yes, there's other playing card decks. I'm most familiar with Italian because I learnt Scopa growing up, but I also know of Swiss and German. I'm sure theres others too), so we'll create a data constructor for the 4 suits and 13 ranks:

```haskell
data Suit = 
  Spades
  | Clubs
  | Diamonds
  | Hearts

data Rank =
  Ace
  | King
  | Queen
  | Jack
  | Ten
  | Nine
  | Eight
  | Seven
  | Six
  | Five
  | Four
  | Three
  | Two
```

Then we define a data constructor for cards, using a rank and suit. I'll also add a separate constructor function `card` which just delegates to the data constructor `Card`, but it's good practice since we can hide the original data constructor when exporting from this module. This'll come in more helpful when we want to write smart constructors for our types, which we'll be doing later.

```haskell
data Card = Card Rank Suit

card :: Rank -> Suit -> Card
card = Card
```

And we can create some cards now:

```haskell
aceOfSpades :: Card
aceOfSpades = card Ace Spades

threeOfHearts:: Card
threeOfHearts = card Three Hearts
```

Now that we have playing cards, we need to be able to create hands out of them. We _could_ just use a `List Card` for poker hands, but a Haskell list can have 0 to infinity elements in it, while a poker hand legally only has 1 to 5 cards, so let's define yet another new domain type. This is also nicer from a semantics point of view—we don't play a list of cards, we play a poker hand:

```haskell
data PokerHand =
  OneCard Card
  | TwoCards Card Card
  | ThreeCards Card Card Card
  | FourCards Card Card Card Card
  | FiveCards Card Card Card Card Card

pair :: PokerHand
pair = HandTwo
  (card Five Clubs)
  (card Five Spades)

royalFlush :: PokerHand
royalFlush = HandFive 
  (card Ace Diamonds)
  (card King Diamonds)
  (card Queen Diamonds)
  (card Jack Diamonds)
  (card Ten Diamonds)
```

TODO: ADD SMART CONSTRUCTORS (SORT BY RANK AND SUIT)

TODO: ADD ORD TO RANK

TODO: ADD CALCULATING DIFFERENCES TO RANK

And now that we have poker hands, it's time to rank them.

```haskell
data PokerHandRank =
  StraightFlush
  | FourOfAKind
  | FullHouse
  | Flush
  | Straight
  | ThreeOfAKind
  | TwoPair
  | Pair
  | HighCard
```

When it comes to ranking poker hands, we'll write some function with the shape `PokerHand -> PokerHandRank`. It'll be easier if we split up the checking logic so we have a function that checks if a hand is a flush, a function that checks if a hand contains a pair, and so on. We could decide to write those individual functions as predicates of shape `PokerHand -> Bool`, which would look something like:

```haskell
isFlush :: PokerHand -> Bool
isFlush (FiveCards
  (Card _ s)
  (Card _ s)
  (Card _ s)
  (Card _ s)
  (Card _ s)
) = True
isFlush _ = False
```

But I don't like this approach for a couple reasons. Firstly, when we need to combine all the `isFlush`, `isStraight`, etc. functions into a single `rank :: PokerHand -> PokerHandRank` function, we'd have to use a long if-then-else chain which I think is just plain ugly. Secondly, the use of Booleans throws away information—we only get one bit of info, true or false, with no explanation of how we got that bit besides the name of the individual ranking functions. 

If you're particularly astute you'll realise these two reasons are essentially the same. I find the if-then-else chain ugly because of this throwing-away-information problem with Booleans (aka Boolean Blindness TODO: LINK). I think a lot of getting better at programming is about aesthetics and developing a sense of taste. Billions of years of evolution gave us a sense of beauty, we may as well harness it and write beautiful software.

Anyways, Haskell offers a really rich typing system, and we can do better than just True and False, we can embed provenance metadata via types to encode more meaning into our functions. We'll write each individual rank function (our `isFlush`, `isPair`, and so on) in the shape `PokerHand -> Maybe PokerHandRank`. If the poker hand fits the rank function, we'll return a `Just PokerHandRank`, otherwise we'll return a `None`. This'll be easier to understand with an example, so lets look at flushes again:

```haskell
rankFlush :: PokerHand -> Maybe PokerHandRank
rankFlush (FiveCards
  (Card _ s)
  (Card _ s)
  (Card _ s)
  (Card _ s)
  (Card _ s)
) = Just Flush
rankFlush _ = None
``` 

Really, we're still returning only a bit of information, since `rankFlush` can only return one of two options, so yes, the function is isomorphic to a predicates that returns a boolean. But now the function is telling us the provenance of that bit—yes, this hand is a flush, or no, there's no flush. We're not throwing that information away anymore; in the predicate approach, the only way to know which rank our hand is is to look at the name of the `isRank` function we just called. Now, we have that information embedded into the type system.

So now when we want to find the highest rank, we'll run the hand through a list of rank membership functions, then return the highest rank in that list. I'm envisioning that we'll run the played poker hand through a list of poker hand rank membership functions `PokerHand -> Maybe PokerHandRank`, returning the list `[Maybe PokerHandRank]`. We can filter this for only the `Just PokerHandRank` entries using `catMaybes` to get the list `[PokerHandRank]`, which we want to find the maximum of. But this could be an empty list (where we'd want to return the rank `HighCard`), so we'll write a nice little helper function to find a maximum value (or default value in case of an empty list) in a generic list:

```haskell
maxOrDefault :: Ord a => a -> [a] -> a
maxOrDefault default = (fromMaybe default) . max

maxPokerHandRank :: [PokerHandRank] -> PokerHandRank
maxPokerHandRank = maxOrDefault HighCard
```

### All the Rank Memberships

Alright, let's not put this off forever, let's write the individual rank membership functions:

```haskell
rankStraightFlush :: PokerHand -> Maybe PokerHandRank
rankStraightFlush hand = rankStraightFlush' (rankStraight hand) (rankFlush hand) where
  rankStraightFlush' :: Maybe PokerHandRank-> Maybe PokerHandRank -> Maybe PokerHandRank
  rankStraightFlush' (Just Straight) (Just Flush) = Just StraightFlush
  rankStraightFlush' _ = None

rankFullHouse :: PokerHand -> Maybe PokerHandRank
rankFullHouse (FiveCards
  (Card rank1 _)
  (Card rank1 _)
  (Card rank1 _)
  (Card rank2 _)
  (Card rank2 _)
) = Just FullHouse
rankFullHouse (FiveCards
  (Card rank1 _)
  (Card rank1 _)
  (Card rank2 _)
  (Card rank2 _)
  (Card rank2 _)
) = Just FullHouse
rankFullHouse _ = None

rankFourOfAKind :: PokerHand -> Maybe PokerHandRank
rankFourOfAKind (FourCards
  (Card rank _)
  (Card rank _)
  (Card rank _)
  (Card rank _)
) = Just FourOfAKind
rankFourOfAKind _ = None

rankFlush :: PokerHand -> Maybe PokerHandRank
rankFlush (FiveCards
  (Card _ suit)
  (Card _ suit)
  (Card _ suit)
  (Card _ suit)
  (Card _ suit)
) = Just Flush
rankFlush _ = None

rankStraight :: PokerHand -> Maybe PokerHandRank
rankStraight (FiveCards
  (Card Ace _ )
  (Card Five _)
  (Card Four _)
  (Card Three _)
  (Card Two _)
) = Just Straight
rankStraight hand =

rankThreeOfAKind :: PokerHand -> Maybe PokerHandRank
rankThreeOfAKind (ThreeCards
  (Card rank _)
  (Card rank _)
  (Card rank _)
) = Just ThreeOfAKind
rankThreeOfAKind _ = None

rankTwoPair: PokerHand -> Maybe PokerHandRank
rankTwoPair (FourCards
  (Card rank1 _)
  (Card rank1 _)
  (Card rank2 _)
  (Card rank2 _)
) = Just TwoPair
rankTwoPair _ = None

rankPair :: PokerHand -> Maybe PokerHandRank
rankPair (TwoCards
  (Card rank _)
  (Card rank _)
) = Just Pair
rankPair _ = None
```

TODO: ADD FN `a -> [(a ->b)] -> [b]`

## Expanding our domain into Balatro

TODO: CHIPS + MULT TYPE

TODO: ADD CHIPS + ADD MULT + SCALE MULT TYPE

TODO: LUCKY CARDS WITH PROBABILITY/OUTCOME SCORING
