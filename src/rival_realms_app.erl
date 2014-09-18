-module(rival_realms_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    rival_realms_sup:start_link().

stop(_State) ->
    ok.
