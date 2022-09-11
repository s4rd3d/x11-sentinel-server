%%%-----------------------------------------------------------------------------
%%% @doc TODO
%%% @end
%%%-----------------------------------------------------------------------------

-module(xss_database_server).
-include("x11_sentinel_server.hrl").
-behaviour(gen_server).

%%%=============================================================================
%%% Exports
%%%=============================================================================

%% `gen_server' API
-export([start_link/0]).

%% `gen_server' callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         terminate/2
        ]).

%% Exported functions
-export([execute/1]).

%%%=============================================================================
%%% Macros
%%%=============================================================================

-define(TIMEOUT, 5000). % 5 seconds

%%%=============================================================================
%%% Records
%%%=============================================================================

%% Internal `gen_server' state
-record(state, {connection :: epgsql:connection()}).

%%%=============================================================================
%%% Types
%%%=============================================================================

%% Internal `gen_server' state
-type state() :: #state{}.

%%%=============================================================================
%%% Exported functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Execute a query on the database.
%% @end
%%------------------------------------------------------------------------------
-spec execute(Query) -> Reply | [Reply] when
      Query :: epgsql:sql_query(),
      Reply :: epgsql_cmd_equery:response() | epgsql_sock:error().
execute(Query) ->
    gen_server:call(?MODULE, {execute, Query}).

%%------------------------------------------------------------------------------
%% @doc Start `gen_server'.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          [],
                          [{timeout, ?TIMEOUT}]).

%%------------------------------------------------------------------------------
%% @doc Initialize the server.
%% @end
%%------------------------------------------------------------------------------
-spec init(Args) -> {ok, State} when
      Args :: [any()],
      State :: state().
init([]) ->
    % Set up database connection.
    {ok, DbHost} = application:get_env(?APPLICATION, db_host),
    {ok, DbUsername} = application:get_env(?APPLICATION, db_username),
    {ok, DbPassword} = application:get_env(?APPLICATION, db_password),
    {ok, DbPort} = application:get_env(?APPLICATION, db_port),
    {ok, DbName} = application:get_env(?APPLICATION, db_name),

    {ok, Connection} = epgsql:connect(#{host => DbHost,
                                        username => DbUsername,
                                        password => DbPassword,
                                        port => DbPort,
                                        database => DbName}),

    {ok, _InitialState = #state{connection = Connection}}.

%%------------------------------------------------------------------------------
%% @doc Handle synchronous requests.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(Request, From, State) -> Result when
      Request :: term(),
      From :: {pid(), Tag},
      Tag :: term(),
      State :: state(),
      Result :: {reply, Reply, state()} |
                {reply, Reply, state(), timeout()} |
                {noreply, state()} |
                {noreply, state(), timeout()} |
                {stop, Reason, Reply, state()} |
                {stop, Reason, state()},
      Reply :: term(),
      Reason :: term().
handle_call({execute, Query}, _From, #state{connection = Connection} = State) ->
    Reply = epgsql:equery(Connection, Query),
    {reply, Reply, State};
handle_call(Request, From, State) ->
    ok = logger:warning(#{message => "Unhandled handle_call/3",
                          request => Request,
                          from => From,
                          state => State}),
    {reply, {bad_request, ?MODULE, Request}, State}.

%%------------------------------------------------------------------------------
%% @doc Handle asynchronous requests.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(Msg, State) -> Result when
      Msg :: term(),
      State :: state(),
      Result :: {noreply, state()} |
                {noreply, state(), timeout()} |
                {stop, Reason, state()},
      Reason :: term().
handle_cast(Request, State) ->
    ok = logger:warning(#{message => "Unhandled handle_cast/2",
                          request => Request,
                          state => State}),
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @doc Clean up the gen_server state.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(Reason, State) -> any() when
      Reason :: term(),
      State :: state().
terminate(_Reason, #state{connection = Connection}) ->
    ok = epgsql:close(Connection),
    ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
