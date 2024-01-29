Install Rhombus as

 raco pkg install  "https://github.com/mflatt/rhombus-prototype.git#anipict3

to get the "anipict3" branch.

Then, use `make` to create or compile

 play_random  # a player
 play_greedy  # a player
 game.rhm     # the game driver
 slide.rhtm   # a game visualizer

The "game.rhm" program takes player executables as arguments. For
example,

 racket game.rhm play_random play_random

plays a game with two random players. The output is a sequence of JSON
messages sent from the driver (the maps) and back from players (the
individual numbers and strings). Supply the `--base` flag to
"game.rkt" to use all of the base-game cards.

To see the game as a slide presentation, pipe to "slide.rhm":

 racket game.rhm play_random play_random | racket slide.rhm

Since that's a Slideshow presentation, you can also add `-x` to the
end to generate a ".pdf" file.
