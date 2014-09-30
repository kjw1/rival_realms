-module(rr_game).
-export([init_rand/0, init_card_defs/0, new_game/0, get_card/1, shuffle_list/1, play_card/6]).
-export([check_attackers/3, check_defenders/4]).

-ifdef(TEST).
-compile([export_all]).
-endif.

-type card_type() :: supply | unit | realm.
-record(game, {players = []}).
-record(player, {id, hand = [], discard = [], deck = [], lost = [], field = [], life = 20, supply = 0}).
-record(card_def, {key, cost = 0, reqs=[], type = supply :: card_type(), attrs = []}).
-type card_attachment() :: any()
-record(card, {key, type, attached}).

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
  #card_def{key = supply1, attrs=[ {supply, 1 } ]}.
soldier_card() ->
  #card_def{key = soldier, cost = 1, type = unit, attrs=[ {attack, 1}, {defense, 1} ]}.

test_player(Id) ->
  #player{id= Id, deck=shuffle_list(test_deck())}.

init_card_defs() ->
  card_defs = ets:new(card_defs, [public, named_table, {keypos, #card_def.key}]),
  ets:insert(card_defs, supply_card()),
  ets:insert(card_defs, soldier_card()).

get_card(Id) ->
  [#card_def{} = Card] =  ets:lookup(card_defs, Id),
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
              {Player, hand, _Id}, #card_def{type=supply}, {Player, discard}, _Game) ->
  allowed;
%Can deploy units on own deploy phase
%If have enough supply for card
check_allowed(deploy, Player, Player,
              {Player, hand, _Id}, #card_def{cost=Cost, type=unit}, {Player, field}, Game) ->
  #player{supply=Supply} = get_player(Game, Player),
  case Supply of
    Supply when Supply >= Cost -> allowed;
    _Supply -> not_allowed
  end;
%Anything else not allowed
check_allowed(_Phase, _Player, _CurPlayer, _From, _Card, _To, _Game) ->
  not_allowed.

play_card(Phase, Player, From, To, CurPlayer, Game) ->
  Card = card_from_game(From, Game),
  Allowed = check_allowed(Phase, Player, CurPlayer, From, Card, To, Game),
  UpdatedGame = play_card_if_allowed(Allowed, Card, From, To, Game),
  {Allowed, UpdatedGame}.

play_card_if_allowed(not_allowed, _Card, _From, _To, Game) ->
  Game;
play_card_if_allowed(allowed, #card_def{type=unit, cost=Cost}, 
                     From, {Player, field}=To, #game{}=Game) ->
  {Card, RemovedGame} = remove_card(Game, From),
  PaidSupply = add_supply(Player, - Cost, RemovedGame),
  add_card(PaidSupply, Card, To);
play_card_if_allowed(allowed, #card_def{type=supply, attrs=Attrs}, 
                     From, {Player, discard}=To, #game{}=Game) ->
  {supply, Supply} = lists:keyfind(supply, 1, Attrs),
  {Card, RemovedGame} = remove_card(Game, From),
  AddedGame = add_card(RemovedGame, Card, To),
  UpdatedSupply = add_supply(Player, Supply, AddedGame),
  UpdatedSupply.

add_supply(Player, Supply, Game) ->
  PlayerState = get_player(Game, Player),
  SuppliedPlayer = PlayerState#player{supply= PlayerState#player.supply + Supply},
  update_player(Game, Player, SuppliedPlayer).

get_player(#game{players=Players}, Id) ->
  lists:keyfind(Id, #player.id, Players).

update_player(#game{players=Players}= Game, Player, PlayerState) ->
  Game#game{players = lists:keyreplace(Player, #player.id, Players, PlayerState)}.

remove_card(Game, {Player, PileName, Id}) ->
  PlayerState = get_player(Game, Player),
  Pile = pile_for_player(PlayerState, PileName),
  {value, Card, UpdatedPile} = lists:keytake(Id, 1, Pile),
  UpdatedPlayer = update_pile_for_player(PlayerState, PileName, UpdatedPile),
  {Card, update_player(Game, Player, UpdatedPlayer)}.

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

check_defenders(DefPlayer, Attackers, Defenders, Game) ->
  ValidDefenders = check_defenders_on_field(DefPlayer, Defenders, Game),
  ValidAttackers = check_declared_attackers_valid(Attackers, Defenders),
  %%TODO check to see if attributes allow defense etc
  Ok = lists:all(fun(IsAllowed) -> IsAllowed =:= true end, [ValidAttackers, ValidDefenders]),
  bool_to_allow(Ok).

check_declared_attackers_valid(Attackers, Defenders) ->
  DeclaredAttackers = lists:map(fun({_DefenderList, Attacker}) -> Attacker end, Defenders),
  lists:all(fun(DecAttacker) -> 
        lists:any(fun(Attacker) -> Attacker=:= DecAttacker end, Attackers)
    end, DeclaredAttackers).


check_defenders_on_field(Player, Defenders, Game) ->
  DefenderLists = lists:map(fun({DefenderList, _Attacker}) -> DefenderList end, Defenders),
  lists:all(fun(DefenderList) -> 
          allowed =:= check_attackers(Player, DefenderList, Game) 
      end, DefenderLists).

check_attackers(Player, Attackers, Game) ->
  PlayerState = get_player(Game, Player),
  PlayerField = pile_for_player(PlayerState, field),
  bool_to_allow( check_attackers_in_field(PlayerField, Attackers)) .

check_attackers_in_field(Field, Attackers) ->
  try
    lists:all(fun(Attacker) ->
            {Attacker, CardType} = lists:keyfind(Attacker, 1, Field),
            %io:format("Got card: ~p~n", [get_card(CardType)]),
            Card = get_card(CardType),
            unit =:= Card#card_def.type
        end, Attackers)
  catch
    _Error:_Exception ->
      %io:format("Error ocurred: ~p~n", [Exception]),
      false
  end.

bool_to_allow(true) -> allowed;
bool_to_allow(false) -> not_allowed.

