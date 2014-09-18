-module(rr_game_tests).
-include_lib("eunit/include/eunit.hrl").

new_game_test() ->
  {game, [ _Player1, _Player2 ] } = ?debugVal(rr_game:new_game()).

card_def_test() ->
  rr_game:init_card_defs(),
  Soldier = rr_game:soldier_card(),
  Soldier = rr_game:get_card(soldier),
  Supply = rr_game:supply_card(),
  Supply = rr_game:get_card(supply1).
