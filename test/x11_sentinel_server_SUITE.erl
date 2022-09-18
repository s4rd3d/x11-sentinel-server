-module(x11_sentinel_server_SUITE).
-include("x11_sentinel_server.hrl").

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

-compile(export_all).
-compile(nowarn_export_all).

%%%=============================================================================
%%% Types
%%%=============================================================================

%%%=============================================================================
%%% Macros
%%%=============================================================================

%% Test server's default host
-define(DEFAULT_HOST, "localhost").

%% Test server's default port
-define(DEFAULT_PORT, 8084).

%% Database connection options
-define(DB_HOST, "localhost").
-define(DB_USERNAME, "xss").
-define(DB_PASSWORD, "secret").
-define(DB_PORT, 5432).
-define(DB_NAME, "xss").

%% Default entities
-define(DEFAULT_USER_ID, <<"default_user">>).

%%%=============================================================================
%%% CT callback
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Return the test cases.
%% @end
%%------------------------------------------------------------------------------
-spec all() -> Result when
      Result :: [ct_suite:ct_testname()].
all() ->
    [http_connectivity_test,
     db_connectivity_test,
     user_query_test].

%%%-----------------------------------------------------------------------------
%%% Test suite init/end
%%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Initialize before the test suite.
%% @end
%%------------------------------------------------------------------------------
-spec init_per_suite(Config) -> Config when
      Config :: ct_suite:ct_config().
init_per_suite(Config) ->
    ok = logger:set_primary_config(level, debug),
    ok = logger:add_handler(test_logger,
                            logger_std_h,
                            #{level => debug,
                              config => #{file => "log/debug.log"}}),
    {ok, _} = application:ensure_all_started(gun),

    ok = application:set_env(?APPLICATION, db_host, ?DB_HOST),
    ok = application:set_env(?APPLICATION, db_username, ?DB_USERNAME),
    ok = application:set_env(?APPLICATION, db_password, ?DB_PASSWORD),
    ok = application:set_env(?APPLICATION, db_port, ?DB_PORT),
    ok = application:set_env(?APPLICATION, db_name, ?DB_NAME),
    {ok, _} = application:ensure_all_started(?APPLICATION),
    Config.

%%------------------------------------------------------------------------------
%% @doc Clean up after the test suite.
%% @end
%%------------------------------------------------------------------------------
-spec end_per_suite(Config) -> ok when
      Config :: ct_suite:ct_config().
end_per_suite(_Config) ->
    ok = application:stop(?APPLICATION),
    ok = application:stop(gun),
    ok.

%%%-----------------------------------------------------------------------------
%%% Test case init/end
%%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Initialize before a test case.
%% @end
%%------------------------------------------------------------------------------
-spec init_per_testcase(Testcase, Config) -> Config when
      Testcase :: ct_suite:ct_testname(),
      Config :: ct_suite:ct_config().
init_per_testcase(_Testcase, Config) ->
    Config.

%%------------------------------------------------------------------------------
%% @doc Clean up after a test case.
%% @end
%%------------------------------------------------------------------------------
-spec end_per_testcase(Testcase, Config) -> ok when
      Testcase :: ct_suite:ct_testname(),
      Config :: ct_suite:ct_config().
end_per_testcase(_Testcase, _Config)->
    % Clean up database
    ok = xss_database_server:empty_tables(),
    ok.

%%%=============================================================================
%%% Test cases
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Check the availability of the web server over HTTP.
%% @end
%%------------------------------------------------------------------------------
-spec http_connectivity_test(Config) -> ok when
      Config :: ct_suite:ct_config().
http_connectivity_test(_Config) ->
    {ok, ConnPid} = gun:open(
                      ?DEFAULT_HOST,
                      application:get_env(?APPLICATION, port, ?DEFAULT_PORT)),
    StreamRef = gun:head(ConnPid, "/"),
    ?assertMatch({response, fin, _, _Headers}, gun:await(ConnPid, StreamRef)),
    ok.

%%------------------------------------------------------------------------------
%% @doc Check the database connection by a simple query.
%% @end
%%------------------------------------------------------------------------------
-spec db_connectivity_test(Config) -> ok when
      Config :: ct_suite:ct_config().
db_connectivity_test(_Config) ->
    ?assertMatch({ok, _Columns, [{4}]},
                 xss_database_server:execute_binary(<<"SELECT 2 + 2">>)),
    ok.

%%------------------------------------------------------------------------------
%% @doc Check the user model related database queries.
%% @end
%%------------------------------------------------------------------------------
-spec user_query_test(Config) -> ok when
      Config :: ct_suite:ct_config().
user_query_test(_Config) ->
    % 1. Add a new user to the database
    User1 = xss_user:new(#{user_id => ?DEFAULT_USER_ID}),
    ?assertMatch({ok, 1}, xss_user_store:insert_user(User1)),

    % 2. Select the newly added user
    {ok, User2} = xss_user_store:select_user_by_user_id(?DEFAULT_USER_ID),
    ?assertMatch(#{event_count := 0, user_id := ?DEFAULT_USER_ID}, User2),

    % 3. Update the event count of the user
    {ok, 1} = xss_user_store:update_user_event_count(User2, 42),
    {ok, User3} = xss_user_store:select_user_by_user_id(?DEFAULT_USER_ID),
    ?assertMatch(#{event_count := 42, user_id := ?DEFAULT_USER_ID}, User3),

    % 4. Soft delete user
    {ok, 1} = xss_user_store:soft_delete_user_by_user_id(?DEFAULT_USER_ID),
    {error, Reason} = xss_user_store:select_user_by_user_id(?DEFAULT_USER_ID),
    ?assertEqual(#{reason => <<"User does not exist.">>,
                   user_id => ?DEFAULT_USER_ID},
                 Reason),
    ok.


%%%=============================================================================
%%% Helper functions
%%%=============================================================================
