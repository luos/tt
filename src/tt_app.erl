%%%-------------------------------------------------------------------
%% @doc tt public API
%% @end
%%%-------------------------------------------------------------------

-module(tt_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    tt_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
