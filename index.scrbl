#lang scribble/manual

@(require "cardgame.rkt")
@(require pict racket/math)

@title[#:version ""]{Coin Gremlins Rules}

Coin Gremlins is a game about buying cards,
gaining wealth, and bluffing.
It can be played by 2 or more players and games
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
@(define day-tracker-card
  (render-card day-tracker))

@(define d20 (scale-to-height (bitmap "d20.jpeg") 700))
@(define player-area
  (hc-append padding
   stipend-card
   (ghost stipend-card) d20))

@(scale-to-height
  (vc-append (* padding 2)
    (hc-append padding
      (make-area shop-area-cards "Shop Area")
      (vc-append (* padding 2)
        (make-area day-tracker-card "Day Tracker" #:show-box? #f)
        (make-area (ghost stipend-card) "Discard Pile")))
    (hc-append (* padding 2)  
      (make-area player-area "Player 1 Army Area")
      (make-area player-area "Player 2 Army Area")))
  600)

TODO add numbering to cards

TODO add starting coin for each player

TODO add pile of coins

TODO add coin on day tracker on day 1


Coin Gremlins has two kinds of play areas.
First, there is the @bold{shop} area, which holds cards players can buy.
Second, each player has a @bold{army} area,
which holds cards the player has bought.
Each player keeps their army area in the
order they bought the cards, from left to right.
Finally, there is also a @bold{discard} pile
for cards that have @bold{feinted}.

@(define person-small (scale-to-height person-image 20))

The base game contains @(number->string (+ (length base-game) (length every-game))) types of cards.
Each player starts with one @code{Stipend} card.
The rest of the cards are placed on the table
in the @bold{shop} area.
Each card has a player count @person-small, which determines how many of
that card is used per person.
For example, in a 3-player game, there will be
6 @code{Stone Wall} cards in the shop.

Each card in the @bold{shop} area is also assigned a number 1-@(number->string (length shop-base-game)).
This number will be used to refer to the card
when players buy it.
Each player also starts the game with 1 coin
and a d20 die.


@section{General Rules}

Coin Gremlins is played in rounds called @bold{days},
each round corresponding to a day in a 3-day cycle.
First day 1, then day 2, then day 3, 
then back to day 1 again.
The current day can be tracked by placing a
coin on the @code{Day Tracker} card.

Each round consists of 3 phases: the @bold{income phase}, the @bold{attack phase}, and the @bold{buy phase}.
The income phase applies effects from each card
each player owns.
The attack phase allows one player to attack 
all other players.
The buy phase allows players to buy new cards.
These phases are explained in the following sections.

@(scale-to-height (render-card poison) 400)

Coin gremlin cards have a @bold{cost} in the lower-left,
a @bold{strength} in the upper-left, and a @bold{defense} in the upper-right.
They also have a @bold{player count} in the lower-right, which
decides how many of that card are used per player.
Cards also have a description of their effect
during the @bold{income phase}.
Sometimes, cards have special effects during
other phases, such as the @bold{attack phase}.
In such cases, the description of the effect
will explicitly state when it applies.


Some things in this game are revealed @bold{simultaneously}.
This means that all players, in secret, decide on what they are going to do.
Simultaneously, all players reveal their decisions.


@section{Income Phase}

During the income phase, each player applies
the effects of each card they own.
All players perform the income phase at the
same time.
However, each player must apply the effects
of their cards in order from left to right.
<<<<<<< HEAD
For example, the @code{income} card gives the player
=======
For example, the @code{Income} gives the player
>>>>>>> bade6ed (capitolize card names)
interest after the cards to its left and before
those on its right.

TODO example

@section{Attack Phase: Investing}

During the @bold{attack phase}, players pay money, @bold{investing} in their army
for the opportunity to @bold{attack} the other players.
Each player secretly decides
how much money they want to spend.
They place that number face up on their d20,
and keep the die hidden.
@bold{Simultaneously}, all players reveal their
d20s and pay the amount shown.
All players must pay at least 1 coin.

If one player paid more than all other players,
they @bold{attack} the other players.
For example, if player A paid 3 coins, player B paid 2 coins, and player C paid 2 coins,
player A gets to attack the other players.
In the case of a tie, no player gets to attack.


@section{Attack Phase: Attacking}

The attacking player may attack with each of
their cards once, in the order of their choosing.
Likewise, each defending player may defend
with each of their cards once (except for the stone wall card).
After a card attacks or defends, it is @bold{tapped}
(turned sideways) to indicate that it has been used.

Once the attacking player has chosen a card to attack with,
all other defending players must choose a card to defend with.
If a defending player has no untapped cards,
@bold{they are eliminated from the game}.
The attacking card @bold{fights} the defending cards.


During a @bold{fight}, the attacking player's card may @bold{feint}
and some or all of the defending cards may @bold{feint}.
If any defending card has a higher attack than the attacking card's defense,
the attacking card feints.
For any defending card, if the attacking card's attack is higher than
the defending card's defense, the defending card feints.
Note that, even if the attacking card feints, it still can cause
defending cards to feint.


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
chose a @code{Poison} card to attack with.
The defending players chose a @code{Poison} card,
an @code{Armadillo} card, and a @code{Farmer} card, respectively.
As a result, the attacking @code{Poison} card @bold{feints},
because the defending @code{Poison} card has a higher
attack than the attacking @code{Poison} card's defense.
Also, the defending @code{Poison} card and @code{Farmer} card
both @bold{feint}, since they have lower defense
than the attacking @code{Poison} card's attack.
Finally, the defending @code{Armadillo} card does not @bold{feint}.

After a @bold{fight}, the tapped cards are placed back
in the army area in the same order they were in before.
Any cards that @bold{feinted} are placed in the discard pile.

@section{Buy Phase}

During the buy phase, players may buy cards from the shop.
Each player gets one free @bold{buy} per round.
Cards, like the @code{Merchant}, can give players more
@bold{buys} during the income phase.

Players buy cards from the shop @bold{simultaneously}.
First, all players decide on a card to buy in secret.
They place their d20s with the number showing the number
of the card they want to buy.
@bold{Simultaneously}, all players reveal their d20s.
Each player pays the price of their chosen card and gains that card,
placing it in their army area to the right of the
cards already there.

If at least one player still has a @bold{buy} left,
this process continues.
Players that still have a @bold{buy} left @bold{simultaneously}
choose another card to buy, and so on until all players
run out of @bold{buys}.

Sometimes, multiple players choose the same card
and there are not enough for all of them left in the shop.
In this case, discard all remaining cards of that type in the shop.
The players that chose that card still spend their money,
@bold{but none of them get the card}.
For example, there could be only two @code{Stone Wall} left
in the shop but three players have chosen to buy it.
All three players would pay @(number->string (card-cost stone-wall)) coin, and the two @code{Stone Wall} cards in the shop would be discarded.

@section{Win Conditions}

There are two ways to win the game.
@itemlist[
  @item{First, if only one player is left in the game, they win.}
  @item{Second, the first player to reach 7 or more victory points instantly wins, as long as they also have more points than every other player.}
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
  @item{Print out TODO this pdf of the cards, and cut them out. It prints enough cards for 4 players.}
  @item{(optional) Sleeve the cards.}
  @item{Get some tokens to use as coins. I use a bunch of old bread clips.}
  @item{Get a d20 die per player.}
]