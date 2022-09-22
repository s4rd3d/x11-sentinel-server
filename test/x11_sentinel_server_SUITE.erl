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

%% Default entity identifiers
-define(DEFAULT_USER_ID, <<"default_user">>).
-define(DEFAULT_SESSION_ID, <<"default_session">>).
-define(DEFAULT_STREAM_ID, <<"default_stream">>).

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
     models_query_test].

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
%% @doc Check the model related database queries. The following models are
%%      tested:
%%
%%      *   `xss_chunk'
%%      *   `xss_session'
%%      *   `xss_stream'
%%      *   `xss_user'
%% @end
%%------------------------------------------------------------------------------
-spec models_query_test(Config) -> ok when
      Config :: ct_suite:ct_config().
models_query_test(_Config) ->
    % 1. Create models with default configurations and save them to the db.
    User1 = xss_user:new(#{user_id => ?DEFAULT_USER_ID}),
    Session1 = xss_session:new(#{session_id => ?DEFAULT_SESSION_ID}),
    Stream1 = xss_stream:new(#{stream_id => ?DEFAULT_STREAM_ID,
                               session_id => ?DEFAULT_SESSION_ID,
                               user_id => ?DEFAULT_USER_ID}),
    Chunk1 = do_create_new_chunk(),

    ?assertMatch({ok, 1}, xss_user_store:insert_user(User1)),
    ?assertMatch({ok, 1}, xss_session_store:insert_session(Session1)),
    ?assertMatch({ok, 1}, xss_stream_store:insert_stream(Stream1)),
    ?assertMatch({ok, 1}, xss_chunk_store:insert_chunk(Chunk1)),

    % 2. Check SELECT queries and assert equality with the original models.
    {ok, User2} = xss_user_store:select_user_by_user_id(?DEFAULT_USER_ID),
    ?assertMatch(#{event_count := 0, user_id := ?DEFAULT_USER_ID}, User2),
    {ok, Session2} =
      xss_session_store:select_session_by_session_id(?DEFAULT_SESSION_ID),
    ?assertMatch(#{session_id := ?DEFAULT_SESSION_ID}, Session2),
    {ok, Stream2} =
      xss_stream_store:select_stream_by_stream_id(?DEFAULT_STREAM_ID),
    ?assertMatch(#{stream_id := ?DEFAULT_STREAM_ID,
                   session_id := ?DEFAULT_SESSION_ID,
                   user_id := ?DEFAULT_USER_ID},
                 Stream2),
    {ok, Chunk2} = xss_chunk_store:select_chunk_by_stream_id_and_sequence_number(?DEFAULT_STREAM_ID, 0),
    ?assertMatch(#{chunk := [],
                   metadata := #{user_id := ?DEFAULT_USER_ID,
                                 session_id := ?DEFAULT_SESSION_ID,
                                 stream_id := ?DEFAULT_STREAM_ID,
                                 sequence_number := 0,
                                 epoch := #{unit := <<"milisecond">>,
                                            value := 0}},
                   real_ip_address := <<"127.0.0.1">>,
                   peer_ip_address := <<"127.0.0.1">>,
                   referer := <<"localhost">>,
                   chunk := []}, Chunk2),

    % 2. Check UPDATE queries.
    {ok, 1} = xss_user_store:update_user_event_count(User2, 42),
    {ok, User3} = xss_user_store:select_user_by_user_id(?DEFAULT_USER_ID),
    ?assertMatch(#{event_count := 42, user_id := ?DEFAULT_USER_ID}, User3),

    % 3. Soft delete entities from the database.
    {ok, 1} = xss_chunk_store:soft_delete_chunk_by_stream_id_and_sequence_number(?DEFAULT_STREAM_ID, 0),
    {error, Reason1} = xss_chunk_store:select_chunk_by_stream_id_and_sequence_number(?DEFAULT_STREAM_ID, 0),
    ?assertEqual(#{reason => <<"Chunk does not exist.">>,
                   stream_id => ?DEFAULT_STREAM_ID,
                   sequence_number => 0},
                 Reason1),
    {ok, 1} = xss_session_store:soft_delete_session_by_session_id(?DEFAULT_SESSION_ID),
    {error, Reason2} = xss_session_store:select_session_by_session_id(?DEFAULT_SESSION_ID),
    ?assertEqual(#{reason => <<"Session does not exist.">>,
                   session_id => ?DEFAULT_SESSION_ID},
                 Reason2),
    {ok, 1} = xss_stream_store:soft_delete_stream_by_stream_id(?DEFAULT_STREAM_ID),
    {error, Reason3} = xss_stream_store:select_stream_by_stream_id(?DEFAULT_STREAM_ID),
    ?assertEqual(#{reason => <<"Stream does not exist.">>,
                   stream_id => ?DEFAULT_STREAM_ID},
                 Reason3),
    {ok, 1} = xss_user_store:soft_delete_user_by_user_id(?DEFAULT_USER_ID),
    {error, Reason4} = xss_user_store:select_user_by_user_id(?DEFAULT_USER_ID),
    ?assertEqual(#{reason => <<"User does not exist.">>,
                   user_id => ?DEFAULT_USER_ID},
                 Reason4),
    ok.


%%%=============================================================================
%%% Helper functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Create a new chunk with the default parameters.
%% @end
%%------------------------------------------------------------------------------
-spec do_create_new_chunk() -> Chunk when
      Chunk :: xss_chunk:chunk().
do_create_new_chunk() ->
    LooseChunk = #{chunk => [],
                   metadata =>
                     #{user_id => ?DEFAULT_USER_ID,
                       session_id => ?DEFAULT_SESSION_ID,
                       stream_id => ?DEFAULT_STREAM_ID,
                       sequence_number => 0,
                       epoch => #{unit => milisecond, value => 0}}},
    xss_chunk:new(LooseChunk, {127, 0, 0, 1}, {127, 0, 0, 1}, <<"localhost">>).
