Install Rhombus as

 raco pkg install rhombus

Install `zuo`, maybe from source using

  https://github.com/racket/zuo

Then, use `zuo` to create or compile players (listed roughly from
worst to best):

 play_never
 play_wrong
 play_random
 play_simple
 play_points
 play_attack
 play_decent
 play_gui     # you provide moves through a GUI
 
 game.rhm         # the game driver
 slide.rhtm       # a game visualizer
 tournament.rhtm  # a tournament runner

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
