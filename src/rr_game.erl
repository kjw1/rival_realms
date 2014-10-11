-module(rr_game).
-export([init_rand/0, init_card_defs/0, new_game/0, get_card_def/1, shuffle_list/1, make_play/4]).
-export([check_attackers/3, check_defenders/4]).

-ifdef(TEST).
-compile([export_all]).
-endif.

-type card_type() :: supply | unit | realm.
-record(game, {players = []}).
-record(player, {id, hand = [], discard = [], deck = [], lost = [], field = [], life = 20, supply = 0}).
-record(cost, {supply= 0, discards = [], burns=[]}).
-record(card_def, {key, cost :: #cost{}, reqs=[], type = supply :: card_type(), attrs = []}).
-type card_attachment() :: tuple().
-record(card, {id, type, attached :: [card_attachment()], attacking=false}).
-record(ability, {phases, cost :: #cost{}, effect}).

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

get_card_def(Id) ->
  [#card_def{} = Card] =  ets:lookup(card_defs, Id),
  Card.

new_game() ->
  #game{players=[test_player(1), test_player(2)]}.

pile_for_player(#player{hand=Hand}, hand) -> Hand;
pile_for_player(#player{discard=Discard}, discard) -> Discard;
pile_for_player(#player{deck=Deck}, deck) -> Deck;
pile_for_player(#player{lost=Lost}, lost) -> Lost;
pile_for_player(#player{field=Field}, field) -> Field.

ability_from_card(#card_def{attrs=Attrs}, AbilityId) ->
  {abilities, Abilities} = proplists:lookup(abillities, Attrs),
  lists:nth(AbilityId, Abilities).

get_card_or_action(play_hand, PlayerId, CardId, #game{} = Game) ->
  card_from_game({PlayerId, hand, CardId}, Game);
get_card_or_action(activate, _PlayerId, {CardLocation, AbilityId}, #game{} = Game) ->
  #card{type=CardDef} = card_from_game(CardLocation, Game),
  ability_from_card(CardDef, AbilityId).

card_from_game({Player, Where, Id}, #game{players=Players}) ->
  PlayerState = lists:keyfind(Player, #player.id, Players),
  Pile = pile_for_player(PlayerState, Where),
  lists:keyfind(Id, #card.id, Pile).

check_controls_card(play_hand, _CardId, _Player) ->
  allowed;
check_controls_card(activate, {{PlayerId, _Pile, _CardId}, _AbilityId}, PlayerId) ->
  allowed;
check_controls_card(_Action, _Source, _PlayerId) ->
  not_allowed.

check_phase(CurPhase, PlayerId, PlayerId, #ability{phases=Phases}) ->
  case lists:any(fun(Phase) -> Phase =:= CurPhase end, Phases) of
    true -> allowed;
    false -> not_allowed
  end;
check_phase({reinforce, PlayerId}, PlayerId, _CurPlayer, #ability{phases=Phases}) ->
  case lists:any(fun(Phase) -> Phase =:= reinforce end, Phases) of
    true -> allowed;
    false -> not_allowed
  end;
check_phase({reinforce, PlayerId}, PlayerId, _CurPlayer, #card_def{type=reinforce}) ->
  allowed;
check_phase(deploy, PlayerId, PlayerId, #card_def{type=unit}) ->
  allowed;
check_phase(deploy, PlayerId, PlayerId, #card_def{type=realm}) ->
  allowed.

get_card_or_ability_cost(#ability{cost=Cost}) ->
  Cost;
get_card_or_ability_cost(#card{type=Type}) ->
  #card_def{cost=Cost} = get_card_def(Type),
  Cost.

check_cost(PlayerId, CostTargets, CardOrAbility, #game{}=Game) ->
  Player = get_player(Game, PlayerId),
  #cost{supply=Supply, discards=Discards, burns=Burns} = get_card_or_ability_cost(CardOrAbility),
  HasSupply = check_player_has_supply(Player, Supply),
  CostTargetsOk = check_cost_targets(CostTargets, Discards, Burns),
  HasSupply and CostTargetsOk.

check_player_has_supply(#player{supply=Supply}, SupplyCost) ->
  Supply >= SupplyCost.

%% TODO Fix this later
check_cost_targets({Discards, Burns}, ReqDiscards, ReqBurns) ->
  true.

check_targets(CardOrAction, AbilityTargets, Game) ->
  allowed.

%Can always play supply cards
check_allowed(Phase, {Action, Player, Source, CostTargets, AbilityTargets}, CurPlayer, Game) ->
  CardOrAction = get_card_or_action(Action, Player, Source, Game),
  SourceOk = check_controls_card(Action, Source, Player),
  PhaseOk = check_phase(Phase, Player, CurPlayer, CardOrAction),
  CostOk = check_cost(Player, CostTargets, CardOrAction, Game),
  TargetsOk = check_targets(CardOrAction, AbilityTargets, Game),
  check_all_pass(SourceOk, PhaseOk, CostOk, TargetsOk).

check_all_pass(allowed, allowed, allowed, allowed) -> allowed;
check_all_pass(_Fail0, _Fail1, _Fail2, _Fail3) -> not_allowed.

-type player_id() :: integer().
-type card_id() :: integer().
-type ability_index() :: integer().
-type pile() :: hand | field | discard | deck | lost.
-type card_location() :: {player_id(), pile(), card_id()}.
-type targets() :: [ card_location() ] | none | {player_id(), pile()}.
-type game_action() :: { activate, player_id(), {card_location(), ability_index()}, {targets(), targets()}, targets()} | 
  { play_hand, player_id(), card_id(), { targets(),  targets()}, targets()}.

-spec make_play(atom(), game_action(), player_id(), #game{}) -> {allowed | not_allowed, #game{} }.
make_play(Phase, Action, CurPlayer, Game) ->
  Allowed = check_allowed(Phase, Action, CurPlayer, Game),
  UpdatedGame = play_card_if_allowed(Allowed, Action, Game),
  {Allowed, UpdatedGame}.

play_card_if_allowed(not_allowed, Action, Game) ->
  Game;
play_card_if_allowed(allowed, {Action, Player, Source, CostTargets, AbilityTargets}, #game{}=Game) ->
  CardOrAction = get_card_or_action(Action, Player, Source, Game),
  PaidCost = pay_cost(Player, get_card_or_ability_cost(CardOrAction), CostTargets, Game),
  do_play_effect(Action, Player, CardOrAction, Source, AbilityTargets, PaidCost).

pay_cost(Player, Cost, CostTargets, Game) ->
  Game.

%TODO Actually do the effect of the play
do_play_effect(Action, Player, CardOrAction, Source, AbilityTargets, Game) ->
  Game.

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
            Card = get_card_def(CardType),
            unit =:= Card#card_def.type
        end, Attackers)
  catch
    _Error:_Exception ->
      %io:format("Error ocurred: ~p~n", [Exception]),
      false
  end.

bool_to_allow(true) -> allowed;
bool_to_allow(false) -> not_allowed.

