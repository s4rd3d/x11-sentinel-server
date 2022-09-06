%%%-----------------------------------------------------------------------------
%% @doc x11_sentinel_server application module
%% @end
%%%-----------------------------------------------------------------------------

-module(x11_sentinel_server_app).
-include("x11_sentinel_server.hrl").

-behaviour(application).

%%%=============================================================================
%%% Exports
%%%=============================================================================

% Application callbacks
-export([start/2, stop/1]).

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

%%%-----------------------------------------------------------------------------
%% @doc Start the application
%% @end
%%%-----------------------------------------------------------------------------
-spec start(StartType, StartArgs) -> {ok, Pid} |
                                     {ok, Pid, State} |
                                     {error, Reason} when
      StartType :: application:start_type(),
      StartArgs :: term(),
      Pid :: pid(),
      State :: term(),
      Reason :: term().
start(_StartType, _StartArgs) ->
    x11_sentinel_server_sup:start_link().

%%%-----------------------------------------------------------------------------
%% @doc Stop the application
%% @end
%%%-----------------------------------------------------------------------------
-spec stop(State) -> ok when
      State :: term().
stop(_State) ->
    ok.
