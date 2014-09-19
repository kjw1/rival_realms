-module(rr_game).
-export([init_rand/0, init_card_defs/0, new_game/0, get_card/1, shuffle_list/1, play_card/6]).

-ifdef(TEST).
-compile([export_all]).
-endif.

-type card_type() :: supply | unit | realm.
-record(game, {players = []}).
-record(player, {id, hand = [], discard = [], deck = [], lost = [], field = [], life = 20, supply = 0}).
-record(card, {key, cost = 0, reqs=[], type = supply :: card_type(), attrs = []}).

init_rand() ->
  <<A:32,B:32,C:32>> = crypto:rand_bytes(12),
  random:seed({A, B, C}).

shuffle_list(List) ->
  [ N || {_, N} <- lists:sort([ {random:uniform(), N} || N <- List ]) ].

test_deck() ->
  Supply = [ {Id, supply1} || Id <- lists:seq(1, 20) ],
  Units = [ {Id, soldier} || Id <- lists:seq(21, 40) ],
  Supply ++  Units.

supply_card() ->
  #card{key = supply1, attrs=[ {supply, 1 } ]}.
soldier_card() ->
  #card{key = soldier, cost = 1, type = unit, attrs=[ {attack, 1}, {defense, 1} ]}.

test_player(Id) ->
  #player{id= Id, deck=shuffle_list(test_deck())}.

init_card_defs() ->
  card_defs = ets:new(card_defs, [public, named_table, {keypos, #card.key}]),
  ets:insert(card_defs, supply_card()),
  ets:insert(card_defs, soldier_card()).

get_card(Id) ->
  [#card{} = Card] =  ets:lookup(card_defs, Id),
  Card.

new_game() ->
  #game{players=[test_player(1), test_player(2)]}.

pile_for_player(#player{hand=Hand}, hand) -> Hand;
pile_for_player(#player{discard=Discard}, discard) -> Discard;
pile_for_player(#player{deck=Deck}, deck) -> Deck;
pile_for_player(#player{lost=Lost}, lost) -> Lost;
pile_for_player(#player{field=Field}, field) -> Field.

card_from_game({Player, Where, Id}, #game{players=Players}) ->
  PlayerState = lists:keyfind(Player, #player.id, Players),
  Pile = pile_for_player(PlayerState, Where),
  {Id, Card} = lists:keyfind(Id, 1, Pile),
  get_card(Card).

%Can always play supply cards
check_allowed(_Phase, Player, _CurPlayer,
              {Player, hand, _Id}, #card{type=supply}, {Player, discard}, _Game) ->
  allowed.

play_card(Phase, Player, From, To, CurPlayer, Game) ->
  Card = card_from_game(From, Game),
  Allowed = check_allowed(Phase, Player, CurPlayer, From, Card, To, Game),
  UpdatedGame = play_card_if_allowed(Allowed, Card, From, To, Game),
  {Allowed, UpdatedGame}.

play_card_if_allowed(not_allowed, _Card, _From, _To, Game) ->
  Game;
play_card_if_allowed(allowed, #card{type=supply, attrs=Attrs}, 
                     From, {Player, discard}=To, #game{}=Game) ->
  {supply, Supply} = lists:keyfind(supply, 1, Attrs),
  {Card, RemovedGame} = remove_card(Game, From),
  AddedGame = add_card(RemovedGame, Card, To),
  UpdatedSupply = add_supply(Player, Supply, AddedGame),
  UpdatedSupply.

add_supply(Player, Supply, #game{players=Players}=Game) ->
  PlayerState = lists:keyfind(Player, #player.id, Players),
  SuppliedPlayer = PlayerState#player{supply= PlayerState#player.supply + Supply},
  Game#game{players = lists:keyreplace(Player, #player.id, Players, SuppliedPlayer)}.

remove_card(#game{players=Players}=Game, {Player, PileName, Id}) ->
  PlayerState = lists:keyfind(Player, #player.id, Players),
  Pile = pile_for_player(PlayerState, PileName),
  {value, Card, UpdatedPile} = lists:keytake(Id, 1, Pile),
  UpdatedPlayer = update_pile_for_player(PlayerState, PileName, UpdatedPile),
  {Card, Game#game{players = lists:keyreplace(Player, #player.id, Players, UpdatedPlayer)}}.

add_card(#game{players=Players}=Game, Card, {Player, PileName}) ->
  PlayerState = lists:keyfind(Player, #player.id, Players),
  Pile = pile_for_player(PlayerState, PileName),
  UpdatedPile = [Card | Pile],
  UpdatedPlayer = update_pile_for_player(PlayerState, PileName, UpdatedPile),
  Game#game{players = lists:keyreplace(Player, #player.id, Players, UpdatedPlayer)}.

update_pile_for_player(#player{}=Player, hand, Hand) -> Player#player{hand=Hand};
update_pile_for_player(#player{}=Player, discard, Discard) -> Player#player{discard=Discard};
update_pile_for_player(#player{}=Player, deck, Deck) -> Player#player{deck=Deck};
update_pile_for_player(#player{}=Player, lost, Lost) -> Player#player{lost=Lost};
update_pile_for_player(#player{}=Player, field, Field) -> Player#player{field=Field}.
