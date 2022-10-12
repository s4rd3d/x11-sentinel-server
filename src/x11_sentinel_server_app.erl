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
%%% Macros
%%%=============================================================================

% Default port to listen on
-define(DEFAULT_PORT, 8084).

% Cowboy listener
-define(DEFAULT_LISTENER_NAME, x11_sentinel_server_listener).

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
    Port = application:get_env(?APPLICATION, port, ?DEFAULT_PORT),
    Routes = [{"/api/1/s", xss_submission_rest_handler, #{}},
              {"/api/1/status/:user_id", xss_status_rest_handler, #{}},
              {"/api/1/users", xss_users_rest_handler, #{}},
              {"/api/1/users/:user_id", xss_users_rest_handler, #{}},
              {"/api/1/verifications", xss_verifications_rest_handler, #{}},
              {"/api/1/verifications/:user_id", xss_verifications_rest_handler, #{}},
              {"/api/1/events", xss_events_rest_handler, #{}},
              {"/api/1/events/:user_id", xss_events_rest_handler, #{}},
              {"/api/1/state", xss_state_rest_handler, #{}}],
    DispatchRules = cowboy_router:compile([{'_', Routes}]),
    {ok, _} = cowboy:start_clear(?DEFAULT_LISTENER_NAME,
                                 [{port, Port}],
                                 #{env => #{dispatch => DispatchRules}}),
    Result = x11_sentinel_server_sup:start_link(),

    % Cleanup stuck profile building and verifications from the database
    Now = xss_utils:xss_timestamp_to_epgsql_timestamp(xss_utils:now()),
    {ok, _} = xss_database_server:execute(cleanup_profiles, [Now, Now]),
    {ok, _} = xss_database_server:execute(cleanup_verifications, [Now, Now]),

    Result.

%%%-----------------------------------------------------------------------------
%% @doc Stop the application
%% @end
%%%-----------------------------------------------------------------------------
-spec stop(State) -> ok when
      State :: term().
stop(_State) ->
    ok.
