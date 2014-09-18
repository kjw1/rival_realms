-module(rr_game).
-export([init_rand/0, init_card_defs/0, new_game/0, get_card/1, shuffle_list/1]).

-ifdef(TEST).
-compile([export_all]).
-endif.

-type card_type() :: supply | unit | realm.
-record(game, {players = []}).
-record(player, {hand = [], discard = [], deck = [], field = [], life = 20, supply = 0}).
-record(card, {key, cost = 0, reqs=[], type = supply :: card_type(), attrs = []}).

init_rand() ->
  <<A:32,B:32,C:32>> = crypto:rand_bytes(12),
  random:seed({A, B, C}).

shuffle_list(List) ->
  [ N || {_, N} <- lists:sort([ {random:uniform(), N} || N <- List ]) ].

test_deck() ->
  Supply = [ supply1 || _ <- lists:seq(1, 20) ],
  Units = [ soldier || _ <- lists:seq(1, 20) ],
  Supply ++  Units.

supply_card() ->
  #card{key = supply1, attrs=[ {supply, 1 } ]}.
soldier_card() ->
  #card{key = soldier, cost = 1, type = unit, attrs=[ {attack, 1}, {defense, 1} ]}.

test_player() ->
  #player{deck=shuffle_list(test_deck())}.

init_card_defs() ->
  card_defs = ets:new(card_defs, [public, named_table, {keypos, #card.key}]),
  ets:insert(card_defs, supply_card()),
  ets:insert(card_defs, soldier_card()).

get_card(Id) ->
  [#card{} = Card] =  ets:lookup(card_defs, Id),
  Card.

new_game() ->
  #game{players=[test_player(), test_player()]}.
