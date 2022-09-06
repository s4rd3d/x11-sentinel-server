%%%-----------------------------------------------------------------------------
%% @doc x11_sentinel_server top level supervisor.
%% @end
%%%-----------------------------------------------------------------------------

-module(x11_sentinel_server_sup).
-include("x11_sentinel_server.hrl").
-behaviour(supervisor).

%%%=============================================================================
%%% Exports
%%%=============================================================================

% API
-export([start_link/0]).

% Supervisor callback
-export([init/1]).

%%%=============================================================================
%%% Macros
%%%=============================================================================

-define(TIMEOUT, 5000). % 5 seconds

%%%=============================================================================
%%% API functions
%%%=============================================================================
-spec start_link() -> {ok, Pid} | ignore | {error, Reason} when
      Pid :: pid(),
      Reason :: {already_started, pid()} | {shutdown, term()} | term().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Config = #{}).

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================
-spec init(Config) -> {ok, {SupFlags, ChildSpecs}} | ignore when
      Config :: #{},
      SupFlags :: supervisor:sup_flags(),
      ChildSpecs :: [supervisor:child_spec()].
init(_Config = #{}) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 3,
                 period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.
