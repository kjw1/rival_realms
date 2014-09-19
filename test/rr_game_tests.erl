-module(rr_game_tests).
-include_lib("eunit/include/eunit.hrl").

-type card_type() :: supply | unit | realm.
-record(game, {players = []}).
-record(player, {id, hand = [], discard = [], deck = [], lost = [], field = [], life = 20, supply = 0}).
-record(card, {key, cost = 0, reqs=[], type = supply :: card_type(), attrs = []}).

new_game_test() ->
  #game{players= [ #player{}, #player{} ] } = ?debugVal(rr_game:new_game()).


pile_for_player_test() ->
  hand_pile = rr_game:pile_for_player(#player{hand=hand_pile}, hand).

card_test_() ->
  {setup,
    fun() ->
        rr_game:init_card_defs(),
        ok
    end,
   fun(ok) ->
        ok
    end,
   fun(ok) ->
        [
          fun test_card_from_game/0,
          fun test_card_def/0,
          fun test_check_allowed/0
         ]
    end
  }.

test_check_allowed() ->
  EnoughSupply = #game{players =[#player{id=2}, #player{id=1, supply = 1, hand=[ {12, soldier}]} ]},
  allowed = rr_game:check_allowed(deploy, 1, 1, {1, hand, 12}, 
                                  rr_game:soldier_card(), {1, field}, EnoughSupply),
  %Wrong turn
  not_allowed = rr_game:check_allowed(deploy, 1, 2, {1, hand, 12}, 
                                      rr_game:soldier_card(), {1, field}, EnoughSupply),
  %Wrong phase
  not_allowed = rr_game:check_allowed(atack, 1, 2, {1, hand, 12}, 
                                      rr_game:soldier_card(), {1, field}, EnoughSupply),
  %Wrong area
  not_allowed = rr_game:check_allowed(atack, 1, 2, {1, hand, 12}, 
                                      rr_game:soldier_card(), {1, lost}, EnoughSupply),
  %Wrong player area
  not_allowed = rr_game:check_allowed(atack, 1, 2, {1, hand, 12}, 
                                      rr_game:soldier_card(), {2, field}, EnoughSupply),
  Insufficient = #game{players =[#player{id=2}, #player{id=1, supply = 0, hand=[ {12, soldier}]} ]},
  not_allowed = rr_game:check_allowed(deploy, 1, 1, {1, hand, 12}, 
                                      rr_game:soldier_card(), {1, field}, Insufficient).
  

test_card_from_game() ->
  Game = #game{players =[#player{id=2}, #player{id=1, hand=[ {2, supply1}, {12, soldier}]} ]},
  Soldier = rr_game:card_from_game({1, hand, 12}, Game),
  Soldier = rr_game:soldier_card().

test_card_def() ->
  Soldier = rr_game:soldier_card(),
  Soldier = rr_game:get_card(soldier),
  Supply = rr_game:supply_card(),
  Supply = rr_game:get_card(supply1).

remove_card_test() ->
  Game = #game{players =[#player{id=2}, #player{id=1, hand=[ {1, supply1}, {12, soldier}]} ]},
  {{12, soldier}, NextGame} = rr_game:remove_card(Game, {1, hand, 12}),
  #game{players=[#player{id=2}, #player{id=1, hand=[ {1, supply1}]}]} = NextGame.

add_card_test() ->
  Game = #game{players =[#player{id=2}, #player{id=1, hand=[ {12, soldier}]} ]},
  NewCard = {3, supply1},
  NextGame = rr_game:add_card(Game, NewCard, {1, hand}),
  #game{players=[#player{id=2}, #player{id=1, hand=[ NewCard, {12, soldier}]}]} = NextGame.

