matchmaker
==========

A [matchmaking](http://en.wikipedia.org/wiki/Matchmaking_%28video_games%29)
application for Erlang/OTP.


API
---

The callback module passed to `start_matchmaker/1` or `start_matchmaker/2`
should use the `matchmaker` behaviour.

### Exports


__`start_matchmaker(Module) -> {ok, Matchmaker} | {error, Error}`__

    Module = module()
    Matchmaker = pid()
    Error = term()

Creates a new matchmaker server process using `Module` as callback module.
The callback module is expected to implement the `matchmaker` behaviour.


__`start_matchmaker(Name, Module) -> {ok, Matchmaker} | {error, Error}`__

    Name = atom()
    Module = module()
    Matchmaker = pid()
    Error = term()

Creates a new matchmaker server process registered as `Name` using `Module` as
callback module. The callback module is expected to implement the `matchmaker`
behaviour.


__`find_match(MatchmakerRef, Player, Info) -> ok`__

    MatchmakerRef = atom() | pid()
    Player = pid()
    Info = term()

Registers `Player` at the matchmaker for matching with other players. When a
match is found, a game is started by calling `Module:start_game/2`.


### Callback functions

The callback module must implement the following functions to satisfy the
`matchmaker` behaviour.


__`Module:start_game({Player1, Info1}, {Player2, Info2}) -> ok`__

    Player1 = pid()
    Player2 = pid()
    Info1 = term()
    Info2 = term()

Starts a new game with `Player1` and `Player2`. The players are unregistered
from the matchmaker. `Info1` and `Info2` are the terms passed to `find_match/3`
by player 1 and player 2.
