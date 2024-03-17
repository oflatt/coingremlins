This distribution contains the following executables:

 * `game` - Runs one game for 2-4 given players. A game trace is
     printed to stdout, which is each board state sent to each player
     and each player's move, and including a final state that is
     either "end" (as in the spec) or "forfiet" (due to a fault, no in
     the spec). Run with `--help` for supported arguments.

 * `tournament` - Runs many games, pairing the given players for all
     combinations. The games include including all possible pairs and
     orders; order should not matter, except that earlier players get
     an earlier oppotunity to fault. In addition to the normal output,
     "meta" JSON output reports the names of players and summary
     results, which the `slide` program recognizes in additional to
     normal trace output. Run with `--help` for supported arguments.

 * `slide` - Takes the output from `game` or `tournament` and renders
     it. Use Ctl-G or Cmd-G to jump to game results, and use 1 to jump
     to the start or G (without Ctl or Cmd) to jump to the end.

 * `play_random` and `play_decent` - Player programs.

 * `play_gui` - A player program that starts a GUI so that a human can
     enter moves. You can also use this GUI to connect to a server
     with `play_gui <host> <port> <game_id> <player_count> <my_index>`.
     The <host> and <port> arguments select a server. The <game_id>
     plus <player_count> arguments together select a game that
     multiple players can join. Each player should choose a unique
     <index> between 0 (inclusive) and <player_count> (exclusive).
