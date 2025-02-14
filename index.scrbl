#lang scribble/manual

@(require "cardgame.rkt")
@(require pict racket/math racket/match racket/list
          (only-in scribble/core element style))

@(define (card-name s) (element (style 'sf null) s))

@(define day-tracker-name @card-name{Day Tracker})
@(define pass-name @card-name{Pass})
@(define stipend-name @card-name{Sorcerer's Stipend})
@(define wall-name @card-name{Wall of Wealth})
@(define interest-name @card-name{Magic Bean Stock})
@(define poison-name @card-name{Ghost})
@(define farmer-name @card-name{Worker})
@(define buff-farmer-name @card-name{Senior Worker})
@(define armadillo-name @card-name{Shield of Greed})
@(define merchant-name @card-name{Apprentice})
@(define bomb-spirit-name @card-name{Bubble})

@title[#:version ""]{Economancy Rules}

Modern-day wizards are tired of hiding in remote places,
studying ancient tomes, and practicing magic.
These days, wizards want to make money.
In Economancy, ambitious wizards compete to 
gain economic dominance of the modern world.

Players take on the role of wizards
buying cards to build their army.
Players win by either eliminating
their competition or by gaining 7 victory points,
casting a spell over the economy of the world.
Economancy can be played by 2-4 players and games
typically last 10-20 minutes.


@section{Setup}

@(define padding 200)

@(define (make-area contents name #:show-box? [show-box? #t])
  (define rect
   (rounded-rectangle
       (+ (pict-width contents) padding padding)
       (+ (pict-height contents) padding padding)
       -0.05))
  (vl-append 40
    (area-text name)
    (superimpose padding padding
      contents
      (if show-box?
          rect
          (ghost rect)))))

@(define shop-area-cards
  (make-grid
    (map render-card shop-base-game)
    #:num-columns 5
    #:spacing padding))

@(define stipend-card
  (render-card stipend))
@(define coin (scale-to-height (bitmap "coin.png") 300))

@(define day-tracker-card
  (render-card day-tracker))
@(define day-1-tracker-card
  (superimpose 350 75
    coin
    day-tracker-card))

@(define stack-offset (/ padding 6))
@(define (make-stack picts)
  (match picts
    ['() (blank)]
    [`(,pict) pict]
    [`(,pict ,picts ...)
     (superimpose
      stack-offset stack-offset
      (make-stack picts)
      pict)]))



@(define (player-area player-tag)
  @(define 0-coin-card
    (render-card
     (add-tag zero-coins player-tag)))
  @(define pepper-reference-card
    (render-card
      (add-tag (reference-card pepper) player-tag)))

  (vl-append padding
    (hc-append padding
     stipend-card
     (ghost stipend-card)
     (ghost stipend-card)
     coin
     (ghost coin))
    (hc-append padding
     (make-area (make-stack (make-list 4 0-coin-card)) "Coin Cards" #:show-box? #f)
     (make-area
       (make-stack (make-list 4 pepper-reference-card))
       "Reference Cards" #:show-box? #f))))

@(define (make-pile pict n)
  (cond
    [(zero? n) (blank)]
    [else
     (superimpose
      (random 0 (* padding 5))
      (random 0 (* padding 5))
      pict
      (make-pile pict (sub1 n)))]))

@(define pile-of-coins
  (make-pile coin 80))

@(scale-to-width
  (vc-append (* padding 2)
    (hc-append padding
      (make-area shop-area-cards "Shop Area")
      (vc-append (* padding 2)
        (make-area day-1-tracker-card "Day Tracker" #:show-box? #f)
        (make-area (ghost stipend-card) "Discard Pile"))
      (make-area pile-of-coins "Coin Reserve"))
    (hc-append (* padding 2)  
      (make-area (player-area player-1-tag) "Player 1 Army Area")
      (make-area (player-area player-2-tag) "Player 2 Army Area")))
  1000)


The picture above shows the initial setup
for Economancy.
The shop area stores cards players can buy.
The day tracker card helps keep track of the
current day (marked with a coin).
The discard pile stores cards that have fainted.
The coin reserve stores coins the players
can earn during the @bold{income phase}.

Each player has their own
@bold{army area}, which stores the cards they have bought.
Players keep these cards in the order
they bought them, from left to right.
Players start with one coin and one @stipend-name card.
Each player has @bold{coin cards}, which they
used to signal how much money they are spending
during the @bold{attack phase}.
They also have @bold{reference cards},
used to signal what card they are buying
during the @bold{buy phase}.

@(define person-small (scale-to-height person-image 20))

The base game contains @(number->string (length shop-base-game)) types of cards in the shop.
Each player starts with one @stipend-name card.
The rest of the cards are placed on the table
in the @bold{shop} area.
Each card has a player count @person-small, which determines how many of
that card is used per person.
For example, in a 3-player game, there will be
6 @wall-name cards in the shop.


@section{General Rules}

Economancy is played in rounds called @bold{days},
each round corresponding to a day in a 3-day cycle.
First day 1, then day 2, then day 3, 
then back to day 1 again.
The current day can be tracked by placing a
coin on the @day-tracker-name card.

Each round consists of 3 phases: the @bold{income phase}, the @bold{attack phase}, and the @bold{buy phase}.
The income phase applies effects from each card
each player owns.
The attack phase allows one player to attack 
all other players.
The buy phase allows players to buy new cards.
These phases are explained in the following sections.

@(scale-to-height (render-card poison) 400)

Economancy cards have a @bold{cost} in the lower-left,
a @bold{strength} in the upper-left, and a @bold{defense} in the upper-right.
They also have a @bold{player count} in the lower-right, which
determines how many of that card are used per player.
Cards also have a description of their effects
during the @bold{income phase}.
Sometimes, cards have special effects during
other phases, such as the @bold{attack phase}.
In such cases, the description of the effect
will explicitly state when it applies.


@section{Income Phase}

During the income phase, each player applies
the effects of each card they own.
All players perform the income phase at the
same time.
However, each player must apply the effects
of their cards in order from left to right.
For example, the @interest-name card gives the player
coins after the cards to its left and before
those on its right.

For example, if a player currently 0 coins at the start of day 1 and
has @stipend-name, @farmer-name, @interest-name, and @buff-farmer-name
cards acquired in that order, then @stipend-name adds one coin,
@farmer-name adds no coins (because it's day 1), @interest-name adds
no coins (because the player has only 2 coins at that point), and
@buff-farmer-name adds 1 coin, leaving the player with a total of 3
coins. Had the player started day 1 with 1 coin, instead, then
@interest-name would have added a coin (since the player would have 3
coins at the point of considering the card), leaving the player with a
total of 5 coins.

@section{Attack Phase: Investing}

During the @bold{attack phase}, players pay money, @bold{investing} in their army
for the opportunity to @bold{attack} the other players.
They do this by choosing, secretly, how much
money they want to spend.
First, players pick up all of their coin cards
and hold them in their hand.
Then each player picks one or more coin cards
from their hand and places them face down
on the table.
When all players have chosen their coin cards,
they @bold{simultaneously} reveal them.
Each player pays the sum of the number of
coins on the coin cards they revealed.


If one player paid more than all other players,
they @bold{attack} the other players.
For example, if player A paid 3 coins,
player B paid 2 coins,
and player C paid 2 coins,
player A gets to attack the other players.
In the case of a tie, no player gets to attack.


@section{Attack Phase: Attacking}

The attacking player may attack with each of
their cards at most once (except as noted on a card, such as the
@bomb-spirit-name card), in the order of their choosing.
Likewise, each defending player may defend
with each of their cards at most once (except as noted on a card,
such as the @wall-name card).
After a card attacks or defends, it is @bold{tapped}
(turned sideways) to indicate that it has been used. The attacking player
may choose not to attack with some cards or choose
not to attack at all.
After the attack phase, all cards are untapped.

Each time the attacking player chooses a card to attack,
all other defending players secretly choose a card to defend with.
Each player chooses a reference card, placing it face down on the table.
Simultaneously, all players reveal their choices.
If a defending player has no untapped cards to defend an attack,
@bold{they are eliminated from the game}.
If a player has multiple copies of a card, they may choose any of them to defend with.
The attacking card @bold{fights} the defending cards.


During a @bold{fight}, the attacking player's card may @bold{faint}
and some or all of the defending cards may @bold{faint}.
If any defending card has a higher attack than the attacking card's defense,
the attacking card faints.
For any defending card, if the attacking card's attack is higher than
the defending card's defense, the defending card faints.
Note that, even if the attacking card faints, it still can cause
defending cards to faint.


@(define poison-card (render-card poison))
@(define armadillo-card (render-card armadillo))
@(define farmer-card (render-card farmer))
@(define armadillo-card-upside-down (rotate armadillo-card pi))
@(define farmer-card-upside-down (rotate farmer-card pi))
@(define poison-upside-down (rotate poison-card pi))

@(scale-to-height
  (vc-append padding
    (hc-append padding
      poison-upside-down 
      armadillo-card-upside-down
      farmer-card-upside-down)
    poison-card)
  400)

In the above example, the attacking player
chose a @poison-name card to attack with.
The defending players chose a @poison-name card,
an @armadillo-name card, and a @farmer-name card, respectively.
As a result, the attacking @poison-name card @bold{faints},
because the defending @poison-name card has a higher
attack than the attacking @poison-name card's defense.
Also, the defending @poison-name card and @farmer-name card
both @bold{faint}, since they have lower defense
than the attacking @poison-name card's attack.
Finally, the defending @armadillo-name card does not @bold{faint}.

After a @bold{fight}, the tapped cards are placed back
in the army area in the same order they were in before.
Any cards that @bold{fainted} are placed in the discard pile.

@section{Buy Phase}

During the buy phase, players may buy cards from the shop.
Each player gets one free @bold{buy} per round.
Some cards, like the @merchant-name card, can give players more
@bold{buys} during the income phase.
All players buy from the shop simultaneously
so that no player knows what the others
are buying.


To do this, all players first pick up
all their reference cards.
Then, each player with a buy remaining
chooses one reference card
and places it face down on the table.
@bold{Simultaneously}, all players reveal their 
choice.
Each player pays the price of their chosen card and gains that card,
placing it in their army area to the right of the
cards already there.

Note that if a player does not wish to buy a
card, they can place the @pass-name reference card. However, the pass reference card
still uses a @bold{buy}.


If at least one player still has a @bold{buy} left,
this process continues.
Only players that still have a @bold{buy} left
pick another reference card.
Once all players have run out of @bold{buys},
the buy phase ends.

Sometimes, multiple players choose the same card
and there are not enough for all of them left in the shop.
In this case, discard all remaining cards of that type in the shop.
The players that chose that card still spend their money,
@bold{but none of them get the card}.
For example, there could be only two @wall-name left
in the shop but three players have chosen to buy it.
All three players would pay @(number->string (card-cost stone-wall)) coin, and the two @wall-name cards in the shop would be discarded.

@section{Win Conditions}

There are two ways to win the game.
@itemlist[
  @item{If only one player is left in the game, they win.}
  @item{The first player to reach 7 or more victory points instantly wins, as long as they also have more points than every other player.}
]

Multiple players can gain
7 or more victory points at the same time.
In that case, play continues until one has the most points (or until only one player is left).

Ties are also possible, in the case that
the players exhaust the supply of victory points
and are unable to eliminate each other.

@section{How To Make a Physical Copy}

To make a physical copy of this game, you will need to:
@itemlist[
  @item{Print out @link["base-game.pdf"]{this pdf} of the cards, and cut them out. It prints enough cards for 4 players.}
  @item{(optional) Print out @link["booster1.pdf"]{booster pack 1}.}
  @item{(optional) Sleeve the cards, reinforcing using other cards behind the printed cards.}
  @item{Get some tokens to use as coins. I use a bunch of old bread clips.}
]