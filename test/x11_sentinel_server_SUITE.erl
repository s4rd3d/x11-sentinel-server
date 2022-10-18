-module(x11_sentinel_server_SUITE).
-include("x11_sentinel_server.hrl").

-include_lib("eunit/include/eunit.hrl").
-include_lib("common_test/include/ct.hrl").

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
-define(DEFAULT_PORT, 8085).

%% Database connection options
-define(DB_HOST, "localhost").
-define(DB_USERNAME, "xss").
-define(DB_PASSWORD, "secret").
-define(DB_PORT, 5432).
-define(DB_NAME, "xss").

%% Default model identifiers and data
-define(DEFAULT_USER_ID, <<"user@test">>).
-define(DEFAULT_SESSION_ID, <<"default-session">>).
-define(DEFAULT_STREAM_ID, <<"default-stream">>).
-define(DEFAULT_PROFILE_ID, <<"default-profile">>).
-define(DEFAULT_PROFILE_DATA, <<"default-profile-data">>).
-define(DEFAULT_VERIFICATION_ID, <<"default-verification">>).

%% Applications defaults
-define(MINIMUM_EVENT_COUNT_FOR_PROFILE, 1000).
-define(MINIMUM_EVENT_COUNT_FOR_VERIFICATION, 10).
-define(MINIMUM_ELAPSED_TIME_FOR_FAILED_PROFILE_REBUILD, 2000000). % 2 seconds
-define(EVALUATION_SERVICE_HOST, "localhost").
-define(EVALUATION_SERVICE_PORT, 8081).

%% Mock data
-define(VERIFICATION_RESULT, 0.42).

%% Helper function defaults
-define(SLEEP_PERIOD, 500).
-define(WAIT_TIMEOUT, 5000).

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
     models_query_test,
     chunk_submission_test,
     decide_action_test,
     status_rest_handler_test,
     users_rest_handler_test].

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

    ok = application:set_env(?APPLICATION, port, ?DEFAULT_PORT),
    ok = application:set_env(?APPLICATION, db_host, ?DB_HOST),
    ok = application:set_env(?APPLICATION, db_username, ?DB_USERNAME),
    ok = application:set_env(?APPLICATION, db_password, ?DB_PASSWORD),
    ok = application:set_env(?APPLICATION, db_port, ?DB_PORT),
    ok = application:set_env(?APPLICATION, db_name, ?DB_NAME),
    ok = application:set_env(?APPLICATION,
                             minimum_event_count_for_profile,
                             ?MINIMUM_EVENT_COUNT_FOR_PROFILE),
    ok = application:set_env(?APPLICATION,
                             minimum_event_count_for_verification,
                             ?MINIMUM_EVENT_COUNT_FOR_VERIFICATION),
    ok = application:set_env(?APPLICATION,
                             minimum_elapsed_time_for_failed_profile_rebuild,
                             ?MINIMUM_ELAPSED_TIME_FOR_FAILED_PROFILE_REBUILD),
    ok = application:set_env(?APPLICATION,
                             evaluation_service_host,
                             ?EVALUATION_SERVICE_HOST),
    ok = application:set_env(?APPLICATION,
                             evaluation_service_port,
                             ?EVALUATION_SERVICE_PORT),

    {ok, _} = application:ensure_all_started(meck),
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
init_per_testcase(status_rest_handler_test, _Config) ->
    meck:new(xss_api_server, [unstick, passthrough]),
    meck:expect(xss_api_server,
                handle_cast,
                fun
                  ({build_profile, #{profile_id := ProfileId}}, State) ->
                      {ok, 1} = xss_profile_store:update_profile_success(ProfileId, <<>>),
                      {noreply, State};
                  ({verify, #{profile_id := ProfileId,
                              user_id := UserId}}, State) ->
                      {ok, Stream} = xss_stream_store:select_latest_stream_by_user_id(UserId),
                      StreamId = xss_stream:get_stream_id(Stream),
                      VerificationId = xss_utils:generate_uuid(),
                      Verification = xss_verification:new(#{verification_id => VerificationId,
                                                            profile_id => ProfileId,
                                                            stream_id => StreamId,
                                                            last_chunk => 0,
                                                            chunk_count => 0}),
                      {ok, 1} = xss_verification_store:insert_verification(Verification),
                      {ok, 1} = xss_verification_store:update_verification_success(VerificationId, ?VERIFICATION_RESULT),
                      {noreply, State};
                  (Request, State) ->
                      meck:passthrough([Request, State])
                end),
    init_per_testcase_common();
init_per_testcase(_Testcase, _Config) ->
    init_per_testcase_common().

init_per_testcase_common() ->
    % Initialize counter reference
    Counter = counters:new(1, []),
    [{counter, Counter}].

%%------------------------------------------------------------------------------
%% @doc Clean up after a test case.
%% @end
%%------------------------------------------------------------------------------
-spec end_per_testcase(Testcase, Config) -> ok when
      Testcase :: ct_suite:ct_testname(),
      Config :: ct_suite:ct_config().
end_per_testcase(status_rest_handler_test, _Config) ->
    % Unload mocked modules
    meck:unload(),
    end_per_testcase_common();
end_per_testcase(_Testcase, _Config)->
    end_per_testcase_common().

end_per_testcase_common() ->
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
    ok = gun:shutdown(ConnPid),
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
%%      *   `xss_profile'
%%      *   `xss_verification'
%% @end
%%------------------------------------------------------------------------------
-spec models_query_test(Config) -> ok when
      Config :: ct_suite:ct_config().
models_query_test(Config) ->
    % Get counter reference from test configuration.
    Counter = ?config(counter, Config),

    % 1. Create models with default configurations and save them to the db.
    User1 = xss_user:new(#{user_id => ?DEFAULT_USER_ID}),
    Session1 = xss_session:new(#{session_id => ?DEFAULT_SESSION_ID}),
    Stream1 = xss_stream:new(#{stream_id => ?DEFAULT_STREAM_ID,
                               session_id => ?DEFAULT_SESSION_ID}),
    Chunk1 = do_create_new_chunk(Counter),
    Profile1 = xss_profile:new(#{profile_id => ?DEFAULT_PROFILE_ID,
                                 user_id => ?DEFAULT_USER_ID}),
    Verification1 =
      xss_verification:new(#{verification_id => ?DEFAULT_VERIFICATION_ID,
                             profile_id => ?DEFAULT_PROFILE_ID,
                             stream_id => ?DEFAULT_STREAM_ID,
                             last_chunk => 0,
                             chunk_count => 1}),

    ?assertMatch({ok, 1}, xss_user_store:insert_user(User1)),
    ?assertMatch({ok, 1}, xss_session_store:insert_session(Session1)),
    ?assertMatch({ok, 1}, xss_stream_store:insert_stream(Stream1)),
    ?assertMatch({ok, 1}, xss_chunk_store:insert_chunk(Chunk1)),
    ?assertMatch({ok, 1}, xss_profile_store:insert_profile(Profile1)),
    ?assertMatch({ok, 1}, xss_verification_store:insert_verification(Verification1)),

    % Insert connection tables
    ?assertMatch({ok, 1}, xss_database_server:execute(insert_user_session,
                                                      [?DEFAULT_USER_ID,
                                                       ?DEFAULT_SESSION_ID])),
    ?assertMatch({ok, 1}, xss_database_server:execute(insert_user_stream,
                                                      [?DEFAULT_USER_ID,
                                                       ?DEFAULT_STREAM_ID])),

    % 2. Check SELECT queries and assert equality with the original models.

    % user
    {ok, User2} = xss_user_store:select_user_by_user_id(?DEFAULT_USER_ID),
    ?assertMatch(#{event_count := 0, user_id := ?DEFAULT_USER_ID}, User2),

    % session
    {ok, Session2} =
      xss_session_store:select_session_by_session_id(?DEFAULT_SESSION_ID),
    ?assertMatch(#{session_id := ?DEFAULT_SESSION_ID}, Session2),

    % stream
    {ok, Stream2} =
      xss_stream_store:select_stream_by_stream_id(?DEFAULT_STREAM_ID),
    ?assertMatch(#{stream_id := ?DEFAULT_STREAM_ID,
                   session_id := ?DEFAULT_SESSION_ID},
                 Stream2),
    {ok, Stream3} = xss_stream_store:select_latest_stream_by_user_id(?DEFAULT_USER_ID),
    ?assertEqual(Stream2, Stream3),

    % chunk
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

    {ok, SequenceNumbers} = xss_chunk_store:select_sequence_numbers_by_stream_id(?DEFAULT_STREAM_ID),
    ?assertEqual([0], SequenceNumbers),

    % profile
    {ok, Profile2} =
      xss_profile_store:select_profile_by_profile_id(?DEFAULT_PROFILE_ID),
    ?assertMatch(#{profile_id := ?DEFAULT_PROFILE_ID,
                   user_id := ?DEFAULT_USER_ID,
                   profile_data := undefined,
                   succeeded_at := undefined,
                   failed_at := undefined}, Profile2),
    {ok, Profile3} =
      xss_profile_store:select_latest_profile_by_user_id(?DEFAULT_USER_ID),
    ?assertEqual(Profile2, Profile3),

    % verification
    {ok, Verification2} =
      xss_verification_store:select_verification_by_verification_id(?DEFAULT_VERIFICATION_ID),
    ?assertMatch(#{verification_id := ?DEFAULT_VERIFICATION_ID,
                   profile_id := ?DEFAULT_PROFILE_ID,
                   stream_id := ?DEFAULT_STREAM_ID,
                   last_chunk := 0,
                   chunk_count := 1,
                   result := undefined,
                   succeeded_at := undefined,
                   failed_at := undefined}, Verification2),
    {ok, [Verification3]} =
      xss_verification_store:select_verifications_by_profile_id(?DEFAULT_PROFILE_ID),
    ?assertEqual(Verification2, Verification3),
    ?assertEqual(
      {error, #{reason => verification_not_found,
                user_id => ?DEFAULT_USER_ID}},
      xss_verification_store:select_latest_succeeded_verification_by_user_id(?DEFAULT_USER_ID)),

    % 2. Check UPDATE queries.

    % user
    {ok, 1} = xss_user_store:update_user_event_count(User2, 42),
    {ok, User3} = xss_user_store:select_user_by_user_id(?DEFAULT_USER_ID),
    ?assertMatch(#{event_count := 42, user_id := ?DEFAULT_USER_ID}, User3),

    % profile
    {ok, 1} = xss_profile_store:update_profile_success(?DEFAULT_PROFILE_ID,
                                                       ?DEFAULT_PROFILE_DATA),
    {ok, Profile4} =
      xss_profile_store:select_profile_by_profile_id(?DEFAULT_PROFILE_ID),
    ?assertMatch(#{profile_id := ?DEFAULT_PROFILE_ID,
               user_id := ?DEFAULT_USER_ID,
               profile_data := ?DEFAULT_PROFILE_DATA,
               failed_at := undefined}, Profile4),
    ?assert(xss_profile:get_succeeded_at(Profile4) =/= undefined),
    {ok, 1} = xss_profile_store:update_profile_failure(?DEFAULT_PROFILE_ID),
    {ok, Profile5} =
      xss_profile_store:select_profile_by_profile_id(?DEFAULT_PROFILE_ID),
    ?assert(xss_profile:get_failed_at(Profile5) =/= undefined),

    % verification
    {ok, 1} =
      xss_verification_store:update_verification_success(?DEFAULT_VERIFICATION_ID,
                                                         0.42),
    {ok, Verification4} =
      xss_verification_store:select_latest_succeeded_verification_by_user_id(?DEFAULT_USER_ID),
      ?assertMatch(#{verification_id := ?DEFAULT_VERIFICATION_ID,
                 profile_id := ?DEFAULT_PROFILE_ID,
                 stream_id := ?DEFAULT_STREAM_ID,
                 last_chunk := 0,
                 chunk_count := 1,
                 result := 0.42,
                 failed_at := undefined}, Verification4),
    ?assert(xss_verification:get_succeeded_at(Verification4) =/= undefined),

    {ok, 1} =
      xss_verification_store:update_verification_failure(?DEFAULT_VERIFICATION_ID),
    {ok, Verification5} =
      xss_verification_store:select_verification_by_verification_id(?DEFAULT_VERIFICATION_ID),
    ?assert(xss_verification:get_failed_at(Verification5) =/= undefined),

    % 3. Soft delete entities from the database.

    % chunk
    {ok, 1} = xss_chunk_store:soft_delete_chunk_by_stream_id_and_sequence_number(?DEFAULT_STREAM_ID, 0),
    {error, Reason1} = xss_chunk_store:select_chunk_by_stream_id_and_sequence_number(?DEFAULT_STREAM_ID, 0),
    ?assertEqual(#{reason => chunk_not_found,
                   stream_id => ?DEFAULT_STREAM_ID,
                   sequence_number => 0},
                 Reason1),

    % session
    {ok, 1} = xss_session_store:soft_delete_session_by_session_id(?DEFAULT_SESSION_ID),
    {error, Reason2} = xss_session_store:select_session_by_session_id(?DEFAULT_SESSION_ID),
    ?assertEqual(#{reason => session_not_found,
                   session_id => ?DEFAULT_SESSION_ID},
                 Reason2),

    % stream
    {ok, 1} = xss_stream_store:soft_delete_stream_by_stream_id(?DEFAULT_STREAM_ID),
    {error, Reason3} = xss_stream_store:select_stream_by_stream_id(?DEFAULT_STREAM_ID),
    ?assertEqual(#{reason => stream_not_found,
                   stream_id => ?DEFAULT_STREAM_ID},
                 Reason3),

    % user
    {ok, 1} = xss_user_store:soft_delete_user_by_user_id(?DEFAULT_USER_ID),
    {error, Reason4} = xss_user_store:select_user_by_user_id(?DEFAULT_USER_ID),
    ?assertEqual(#{reason => user_not_found,
                   user_id => ?DEFAULT_USER_ID},
                 Reason4),
    % profile
    {ok, 1} = xss_profile_store:soft_delete_profile_by_profile_id(?DEFAULT_PROFILE_ID),
    {error, Reason5} = xss_profile_store:select_profile_by_profile_id(?DEFAULT_PROFILE_ID),
    ?assertEqual(#{reason => profile_not_found,
                   profile_id => ?DEFAULT_PROFILE_ID},
                 Reason5),
    % verification
    {ok, 1} = xss_verification_store:soft_delete_verification_by_verification_id(?DEFAULT_VERIFICATION_ID),
    {error, Reason6} = xss_verification_store:select_verification_by_verification_id(?DEFAULT_VERIFICATION_ID),
    ?assertEqual(#{reason => verification_not_found,
                   verification_id => ?DEFAULT_VERIFICATION_ID},
                 Reason6),
    ok.

%%------------------------------------------------------------------------------
%% @doc Check the data chunk submission REST handler.
%% @end
%%------------------------------------------------------------------------------
-spec chunk_submission_test(Config) -> ok when
      Config :: ct_suite:ct_config().
chunk_submission_test(_Config) ->
    % Submit a chunk to the server
    {ok, ConnPid} = gun:open(
                      ?DEFAULT_HOST,
                      application:get_env(?APPLICATION, port, ?DEFAULT_PORT)),

    Chunk1 = #{<<"metadata">> => #{<<"streamId">> => ?DEFAULT_STREAM_ID,
                                   <<"userId">> => ?DEFAULT_USER_ID,
                                   <<"sessionId">> => ?DEFAULT_SESSION_ID,
                                   <<"sequenceNumber">> => 0,
                                   <<"epoch">> =>
                                     #{<<"unit">> => <<"millisecond">>,
                                       <<"value">> => 42}},
               <<"chunk">> => [<<"some event">>]},

    Body = jiffy:encode(Chunk1, [force_utf8]),
    StreamRef = gun:post(ConnPid,
                         "/api/1/s",
                         [{<<"content-type">>, <<"application/json">>}],
                         Body),
    ?assertMatch({response, nofin, 200, _Headers},
                  gun:await(ConnPid, StreamRef)),

    % Check if entities are saved to the database
    {ok, User} = xss_user_store:select_user_by_user_id(?DEFAULT_USER_ID),
    {ok, Session} = xss_session_store:select_session_by_session_id(?DEFAULT_SESSION_ID),
    {ok, Stream} = xss_stream_store:select_stream_by_stream_id(?DEFAULT_STREAM_ID),
    {ok, Chunk2} = xss_chunk_store:select_chunk_by_stream_id_and_sequence_number(?DEFAULT_STREAM_ID, 0),

    ?assertMatch(#{user_id := ?DEFAULT_USER_ID,
                   event_count := 1},
                 User),
    ?assertMatch(#{session_id := ?DEFAULT_SESSION_ID}, Session),
    ?assertMatch(#{stream_id := ?DEFAULT_STREAM_ID,
                   session_id := ?DEFAULT_SESSION_ID},
                 Stream),
    ?assertMatch(#{metadata := #{user_id := ?DEFAULT_USER_ID,
                                 session_id := ?DEFAULT_SESSION_ID,
                                 stream_id := ?DEFAULT_STREAM_ID,
                                 sequence_number := 0,
                                 epoch := #{unit := <<"millisecond">>,
                                            value := 42}},
                   real_ip_address := <<"127.0.0.1">>,
                   peer_ip_address := <<"127.0.0.1">>,
                   referer := <<"undefined">>,
                   chunk := [<<"some event">>]},
                 Chunk2),
    ok.

%%------------------------------------------------------------------------------
%% @doc Test the profile building business logic.
%%
%%      The test scenario is structured as follows:
%%
%%      1.  Register entities into to database.
%%
%%      2.  Add an empty chunk to the database registered to the test user.
%%
%%      3.  Check the action that needs to be performed, it should be
%%          `not_enough_data_for_profile_building'.
%%
%%      4.  Update the event count of the user to have enough data to build a
%%          profile.
%%
%%      5.  Check the action that needs to be performed, it should be
%%          `build_profile'.
%%
%%      6.  Add a profile to the database which is not built yet (i.e.
%%          `succeded_at' and `failed_at' fields are undefined).
%%
%%      7.  Check the action that needs to be performed, it should be
%%          `profile_building_is_in_progress'.
%%
%%      8.  Update the profile's `failed_at' field to now.
%%
%%      9.  Check the action that needs to be performed, it should be
%%          `cannot_rebuild_new_profile'.
%%
%%      10. Wait for the defined amount of time for profile rebuilding (it is
%%          defined in the `MINIMUM_ELAPSED_TIME_FOR_FAILED_PROFILE_REBUILD'
%%          macro).
%%
%%      11.  Check the action that needs to be performed, it should be
%%          `build_profile'.
%%
%%      12.  Add a new profile to the database and update the `succeeded_at'
%%           field.
%%
%%      13.  Check the action that needs to be performed, it should be `verify'.
%% @end
%%------------------------------------------------------------------------------
-spec decide_action_test(Config) -> ok when
      Config :: ct_suite:ct_config().
decide_action_test(Config) ->
    % Get counter reference from test configuration.
    Counter = ?config(counter, Config),

    % 1.  Register entities into to database.
    User1 = xss_user:new(#{user_id => ?DEFAULT_USER_ID}),
    Session = xss_session:new(#{session_id => ?DEFAULT_SESSION_ID}),
    Stream = xss_stream:new(#{stream_id => ?DEFAULT_STREAM_ID,
                               session_id => ?DEFAULT_SESSION_ID,
                               user_id => ?DEFAULT_USER_ID}),

    ?assertMatch({ok, 1}, xss_user_store:insert_user(User1)),
    ?assertMatch({ok, 1}, xss_session_store:insert_session(Session)),
    ?assertMatch({ok, 1}, xss_stream_store:insert_stream(Stream)),

    % 2.  Add an empty chunk to the database registered to the test user.
    Chunk1 = do_create_new_chunk(Counter),
    ?assertMatch({ok, 1}, xss_chunk_store:insert_chunk(Chunk1)),

    % 3.  Check the action that needs to be performed.
    ?assertEqual({nop, not_enough_data_for_profile_building},
                 xss_api_server:decide_action(User1)),

    % 4.  Update the eventcount of the user.
    EventCount = ?MINIMUM_EVENT_COUNT_FOR_PROFILE,
    ?assertMatch({ok, 1},
                 xss_user_store:update_user_event_count(User1, EventCount)),
    {ok, User2} = xss_user_store:select_user_by_user_id(?DEFAULT_USER_ID),

    % 5.   Check the action that needs to be performed, it should be
    %      `build_profile'.
    ?assertMatch({build_profile, _Profile}, xss_api_server:decide_action(User2)),

    % 6.   Add a profile to the database which is not built yet.
    Profile1 = xss_profile:new(#{profile_id => ?DEFAULT_PROFILE_ID,
                                user_id => ?DEFAULT_USER_ID}),
    ?assertEqual({ok, 1}, xss_profile_store:insert_profile(Profile1)),

    % 7.  Check the action that needs to be performed, it should be
    %     `profile_building_is_in_progress'.
    ?assertEqual({nop, profile_building_is_in_progress},
                  xss_api_server:decide_action(User2)),

    % 8.  Update the profile's `failed_at' field to now.
    ProfileId1 = xss_profile:get_profile_id(Profile1),
    ?assertEqual({ok, 1}, xss_profile_store:update_profile_failure(ProfileId1)),

    % 9.  Check the action that needs to be performed, it should be
    %     `cannot_rebuild_new_profile'.
    ?assertEqual(xss_api_server:decide_action(User2),
                 {nop, cannot_rebuild_new_profile}),

    % 10. Wait for the defined amount of time for profile rebuilding.
    MilliSeconds = ?MINIMUM_ELAPSED_TIME_FOR_FAILED_PROFILE_REBUILD div 1000 + 1,
    receive
    after
        MilliSeconds ->
            % 11.  Check the action that needs to be performed, it should be
            %      `build_profile'.
            ?assertMatch({build_profile, _Profile},
                          xss_api_server:decide_action(User2))
    end,

    % 12.  Add a new profile to the database and update the `succeeded_at'
    %      field.
    ProfileId2 = xss_utils:generate_uuid(),
    Profile2 = xss_profile:new(#{profile_id => ProfileId2,
                                 user_id => ?DEFAULT_USER_ID}),
    ?assertEqual({ok, 1}, xss_profile_store:insert_profile(Profile2)),
    ?assertEqual({ok, 1}, xss_profile_store:update_profile_success(ProfileId2,
                                                                   <<>>)),

    % 13.  Check the action that needs to be performed, it should be `verify'.
    ?assertMatch({verify, _Profile}, xss_api_server:decide_action(User2)),

    ok.

%%------------------------------------------------------------------------------
%% @doc Test the status REST handler.
%%
%%      The test scenario is structured as follows:
%%
%%      1.  Send an empty chunk to the server.
%%
%%      2.  Send a request to the status REST handler and check the result.
%%          It should be `{"phase": "learn", "value": 0.0}'
%%
%%      3.  Send a chunk to server so that the event count does not exceed the
%%          value of the ?MINIMUM_EVENT_COUNT_FOR_PROFILE macro.
%%
%%      4.  Send a request to the status REST handler and check the result.
%%          It should be `{"phase": "learn", "value": Value}' where `Value' is
%%          between 0.0 and 1.0.
%%
%%      5.  Send a chunks to the server so that the event count exceeds the
%%          value of the ?MINIMUM_EVENT_COUNT_FOR_PROFILE macro.
%%
%%      6.  Wait for the profile building.
%%
%%      7.  Send a request to the status REST handler and check the result.
%%          It should be `{"phase": "learn", "value": 1}'.
%%
%%      8.  Send a chunk to the server so that the event count exceeds the
%%          value of the ?MINIMUM_EVENT_COUNT_FOR_VERIFICATION macro.
%%
%%      9.  Wait for the profile verification.
%%
%%      10. Send a request to the status REST handler and check the result.
%%          It should be `{"phase": "verify", "value": Value}' where `Value'
%%          should math the value of the ?VERIFICATION_RESULT macro.
%%
%%      11. Send a chunk to server with a new stream ID so that the event count
%%          does not exceed the value of the
%%          ?MINIMUM_EVENT_COUNT_FOR_VERIFICATION macro.
%%
%%      12. Send a request to the status REST handler and check the result.
%%          It should be `{"phase": "transition", "value": Value}' where `Value'
%%          is between 0.0 and 1.0.
%%
%%      13. Send a chunk to the server with the new stream ID so that the event
%%          count exceeds the value of the ?MINIMUM_EVENT_COUNT_FOR_VERIFICATION
%%          macro.
%%
%%      14.  Wait for the profile verification.
%%
%%      15. Send a request to the status REST handler and check the result.
%%          It should be `{"phase": "verify", "value": Value}' where `Value'
%%          should math the value of the ?VERIFICATION_RESULT macro.
%%
%%
%% @end
%%------------------------------------------------------------------------------
-spec status_rest_handler_test(Config) -> ok when
      Config :: ct_suite:ct_config().
status_rest_handler_test(Config) ->
    % Get counter reference from test configuration.
    Counter = ?config(counter, Config),

    % 1.  Send an empty chunk to the server.
    ok = do_submit_chunk(do_create_new_chunk(Counter)),

    % 2.  Send a request to the status REST handler and check the result.
    %     It should be `{"phase": "learn", "value": 0.0}'
    ?assertMatch(#{<<"phase">> := <<"learn">>, <<"value">> := 0.0},
                 do_call_status(?DEFAULT_USER_ID, ?DEFAULT_STREAM_ID)),

    % 3.  Send a chunks to the server so that the event count does not exceed
    %     the value of the ?MINIMUM_EVENT_COUNT_FOR_PROFILE macro.
    EventCount1 = ?MINIMUM_EVENT_COUNT_FOR_PROFILE div 2,
    ok = do_submit_chunk(do_create_new_chunk(Counter, EventCount1, #{})),

    % 4.  Send a request to the status REST handler and check the result.
    %     It should be `{"phase": "learn", "value": Value}' where `Value' is
    %     between 0.0 and 1.0.
    ?assertMatch(#{<<"phase">> := <<"learn">>,
                   <<"value">> := Value} when Value > 0.0 andalso Value < 1.0,
                 do_call_status(?DEFAULT_USER_ID, ?DEFAULT_STREAM_ID)),

    % 5.  Send a chunk to the server so that the event count exceeds the
    %     value of the ?MINIMUM_EVENT_COUNT_FOR_PROFILE macro.
    EventCount2 = ?MINIMUM_EVENT_COUNT_FOR_PROFILE,
    ok = do_submit_chunk(do_create_new_chunk(Counter, EventCount2, #{})),

    % 6.  Wait for the profile building.
    ok = wait_until(
      fun() ->
          Result =
            xss_profile_store:select_latest_profile_by_user_id(?DEFAULT_USER_ID),
          case
              Result
          of
            {ok, _Profile} ->
                true;
            {error, _Reason} ->
                false
          end
      end,
      ?SLEEP_PERIOD,
      ?WAIT_TIMEOUT),

    % 7.  Send a request to the status REST handler and check the result.
    %     It should be `{"phase": "learn", "value": 1}'.
    ?assertMatch(#{<<"phase">> := <<"learn">>, <<"value">> := 1},
                 do_call_status(?DEFAULT_USER_ID, ?DEFAULT_STREAM_ID)),

    % 8.  Send a chunk to the server so that the event count exceeds the value
    %     of the ?MINIMUM_EVENT_COUNT_FOR_VERIFICATION macro.
    EventCount3 = ?MINIMUM_EVENT_COUNT_FOR_VERIFICATION,
    ok = do_submit_chunk(do_create_new_chunk(Counter, EventCount3, #{})),

    % 9.  Wait for the profile verification.
    ok = wait_until(
      fun() ->
          Result =
            xss_verification_store:select_latest_succeeded_verification_by_user_id(?DEFAULT_USER_ID),
          case
              Result
          of
            {ok, _Verification} ->
                true;
            {error, _Reason} ->
                false
          end

      end,
      ?SLEEP_PERIOD,
      ?WAIT_TIMEOUT),

    % 10. Send a request to the status REST handler and check the result.
    %     It should be `{"phase": "verify", "value": Value}' where `Value'
    %     should math the value of the ?VERIFICATION_RESULT macro.
    ?assertMatch(#{<<"phase">> := <<"verify">>,
                   <<"value">> := ?VERIFICATION_RESULT},
                 do_call_status(?DEFAULT_USER_ID, ?DEFAULT_STREAM_ID)),

    % 11. Send a chunk to server with a new stream ID so that the event count
    %     does not exceed the value of the
    %     ?MINIMUM_EVENT_COUNT_FOR_VERIFICATION macro.
    NewStreamId = <<"new-stream-id">>,
    EventCount4 = ?MINIMUM_EVENT_COUNT_FOR_VERIFICATION div 2,
    ok = do_submit_chunk(do_create_new_chunk(Counter,
                                             EventCount4,
                                             #{stream_id => NewStreamId})),

    %% 12. Send a request to the status REST handler and check the result.
    %%     It should be `{"phase": "transition", "value": Value}' where `Value'
    %%     is between 0.0 and 1.0.
    ?assertMatch(#{<<"phase">> := <<"transition">>,
                   <<"value">> := Value} when Value > 0.0 andalso Value < 1.0,
                 do_call_status(?DEFAULT_USER_ID, NewStreamId)),

    % 13. Send a chunk to the server with the new stream ID so that the event
    %     count exceeds the value of the ?MINIMUM_EVENT_COUNT_FOR_VERIFICATION
    %     macro.
    EventCount5 = ?MINIMUM_EVENT_COUNT_FOR_VERIFICATION,
    ok = do_submit_chunk(do_create_new_chunk(Counter,
                                             EventCount5,
                                             #{stream_id => NewStreamId})),

    % 14. Wait for the profile verification.
    ok = wait_until(
      fun() ->
          Result =
            xss_verification_store:select_latest_succeeded_verification_by_user_id(?DEFAULT_USER_ID),
          case
              Result
          of
            {ok, #{stream_id := StreamId} = _Verification} ->
                StreamId =:= NewStreamId;
            {error, _Reason} ->
                false
          end

      end,
      ?SLEEP_PERIOD,
      ?WAIT_TIMEOUT),

    % 15. Send a request to the status REST handler and check the result.
    %     It should be `{"phase": "verify", "value": Value}' where `Value'
    %     should math the value of the ?VERIFICATION_RESULT macro.
    ?assertMatch(#{<<"phase">> := <<"verify">>,
                   <<"value">> := ?VERIFICATION_RESULT},
                 do_call_status(?DEFAULT_USER_ID, NewStreamId)),
    ok.

%%------------------------------------------------------------------------------
%% @doc Test the users REST handler.
%% @end
%%------------------------------------------------------------------------------
-spec users_rest_handler_test(Config) -> ok when
      Config :: ct_suite:ct_config().
users_rest_handler_test(_Config) ->
    % Add entities to the database
    lists:foreach(
        fun(N) ->
          UserId = <<"user", N/binary, "@test.com">>,
          SessionId = <<"session-", N/binary>>,
          StreamId = <<"stream-", N/binary>>,
          ProfileId = <<"profile-", N/binary>>,
          User = xss_user:new(#{user_id => UserId}),
          Session = xss_session:new(#{session_id => SessionId}),
          Stream = xss_stream:new(#{stream_id => StreamId, session_id => SessionId}),
          Profile = xss_profile:new(#{profile_id => ProfileId, user_id => UserId}),
          ?assertMatch({ok, 1}, xss_user_store:insert_user(User)),
          ?assertMatch({ok, 1}, xss_session_store:insert_session(Session)),
          ?assertMatch({ok, 1}, xss_stream_store:insert_stream(Stream)),
          ?assertMatch({ok, 1}, xss_profile_store:insert_profile(Profile)),
          ?assertMatch({ok, 1}, xss_user_store:update_user_event_count(User, list_to_integer(binary_to_list(N))))
        end,
        [<<"1">>, <<"2">>, <<"3">>]),
    ?assertMatch(ok,insert_new_verification_to_db(#{profile_id => <<"profile-1">>,
                                                     stream_id => <<"stream-1">>,
                                                     result => 0.1})),
    ?assertMatch(ok, insert_new_verification_to_db(#{profile_id => <<"profile-1">>,
                                                     stream_id => <<"stream-1">>,
                                                     result => 0.2})),
    ?assertMatch(ok, insert_new_verification_to_db(#{profile_id => <<"profile-2">>,
                                                     stream_id => <<"stream-2">>,
                                                     result => 0.3})),
    ?assertMatch(ok, insert_new_verification_to_db(#{profile_id => <<"profile-2">>,
                                                     stream_id => <<"stream-2">>,
                                                     result => 0.4})),
    ?assertMatch(ok, insert_new_verification_to_db(#{profile_id => <<"profile-2">>,
                                                     stream_id => <<"stream-2">>,
                                                     result => 0.5})),
    ?assertMatch(ok, insert_new_verification_to_db(#{profile_id => <<"profile-2">>,
                                                     stream_id => <<"stream-2">>,
                                                     result => 0.6})),
    ?assertMatch(ok, insert_new_verification_to_db(#{profile_id => <<"profile-2">>,
                                                     stream_id => <<"stream-2">>,
                                                     result => 0.7})),
    ?assertMatch(ok, insert_new_verification_to_db(#{profile_id => <<"profile-2">>,
                                                     stream_id => <<"stream-2">>,
                                                     result => 0.8})),
    ?assertMatch(ok, insert_new_verification_to_db(#{profile_id => <<"profile-3">>,
                                                     stream_id => <<"stream-3">>,
                                                     result => 0.9})),

    % Check the result of the "GET api/1/users?threshold=0.42" request
    %
    % * user1 should have 2 incidents
    % * user2 should have 2 incidents
    % * user3 should have 0 incidents
    ?assertMatch(#{<<"result">> := <<"ok">>,
                   <<"users">> := [#{<<"userId">> := <<"user1@test.com">>,
                                     <<"eventCount">> := 1,
                                     <<"verifications">> := 2,
                                     <<"incidents">> := 2},
                                   #{<<"userId">> := <<"user2@test.com">>,
                                     <<"eventCount">> := 2,
                                     <<"verifications">> := 6,
                                     <<"incidents">> := 2},
                                   #{<<"userId">> := <<"user3@test.com">>,
                                     <<"eventCount">> := 3,
                                     <<"verifications">> := 1,
                                     <<"incidents">> := 0}]},
                 do_call_users(<<"0.42">>)),

    % Check the result of the "GET api/1/users/user2@test.com?threshold=0.75"
    % request. User2 should have 6 verifications and 5 incidents.
    ?assertMatch(#{<<"result">> := <<"ok">>,
                   <<"user">> := #{<<"userId">> := <<"user2@test.com">>,
                                   <<"eventCount">> := 2,
                                   <<"verifications">> := 6,
                                   <<"incidents">> := 5}},
                 do_call_user(<<"user2@test.com">>, <<"0.75">>)),

    ok.


%%%=============================================================================
%%% Helper functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Create a new chunk with the default parameters.
%% @end
%%------------------------------------------------------------------------------
-spec do_create_new_chunk(Counter) -> Chunk when
      Counter :: counters:counters_ref(),
      Chunk :: xss_chunk:chunk().
do_create_new_chunk(Counter) ->
    LooseChunk = #{chunk => [],
                   metadata =>
                     #{user_id => ?DEFAULT_USER_ID,
                       session_id => ?DEFAULT_SESSION_ID,
                       stream_id => ?DEFAULT_STREAM_ID,
                       sequence_number => get_next_sequnce_number(Counter),
                       epoch => #{unit => milisecond, value => 0}}},
    xss_chunk:new(LooseChunk, {127, 0, 0, 1}, {127, 0, 0, 1}, <<"localhost">>).

%%------------------------------------------------------------------------------
%% @doc Create a new chunk.
%% @end
%%------------------------------------------------------------------------------
-spec do_create_new_chunk(Counter, EventCount, Config) -> Chunk when
      Counter :: counters:counters_ref(),
      EventCount :: non_neg_integer(),
      Config :: map(),
      Chunk :: xss_chunk:chunk().
do_create_new_chunk(Counter, EventCount, Config) ->
  LooseChunk = #{chunk => lists:seq(1, EventCount),
                 metadata =>
                   #{user_id => maps:get(user_id, Config, ?DEFAULT_USER_ID),
                     session_id => maps:get(session_id, Config, ?DEFAULT_SESSION_ID),
                     stream_id => maps:get(stream_id, Config, ?DEFAULT_STREAM_ID),
                     sequence_number => get_next_sequnce_number(Counter),
                     epoch => #{unit => milisecond, value => 0}}},
  xss_chunk:new(LooseChunk, {127, 0, 0, 1}, {127, 0, 0, 1}, <<"localhost">>).

%%------------------------------------------------------------------------------
%% @doc Submit a chunk to the submission end point.
%% @end
%%------------------------------------------------------------------------------
-spec do_submit_chunk(Chunk) -> ok when
      Chunk :: xss_chunk:chunk().
do_submit_chunk(Chunk) ->
    {ok, ConnPid} = gun:open(
                      ?DEFAULT_HOST,
                      application:get_env(?APPLICATION, port, ?DEFAULT_PORT)),
    Body1 = jiffy:encode(Chunk, [force_utf8]),
    StreamRef = gun:post(ConnPid,
                         "/api/1/s",
                         [{<<"content-type">>, <<"application/json">>}],
                         Body1),
    ?assertMatch({response, nofin, 200, _Headers},
                  gun:await(ConnPid, StreamRef)),
    {ok, Body2} = gun:await_body(ConnPid, StreamRef),
    ?assertMatch(#{<<"response">> := <<"ok">>},
                 jiffy:decode(Body2, [return_maps])),
    ok.

%%------------------------------------------------------------------------------
%% @doc Call the status endpoint with a given user ID and return the parsed
%%      result.
%% @end
%%------------------------------------------------------------------------------
-spec do_call_status(UserId, StreamId) -> Response when
      UserId :: xss_user:user_id(),
      StreamId :: xss_stream:stream_id(),
      Response :: #{binary() => binary() | float()}.
do_call_status(UserId, StreamId) ->
    {ok, ConnPid} = gun:open(
                        ?DEFAULT_HOST,
                        application:get_env(?APPLICATION, port, ?DEFAULT_PORT)),
    StreamRef = gun:get(ConnPid,
                        <<"/api/1/status/", UserId/binary, "/",  StreamId/binary>>),
    ?assertMatch({response, nofin, 200, _Headers},
                  gun:await(ConnPid, StreamRef)),
    {ok, Body} = gun:await_body(ConnPid, StreamRef),
    ok = gun:shutdown(ConnPid),
    jiffy:decode(Body, [return_maps]).

%%------------------------------------------------------------------------------
%% @doc Call the users endpoint with a given threshold and return the parsed
%%      result.
%% @end
%%------------------------------------------------------------------------------
-spec do_call_users(Threshold) -> Response when
      Threshold :: binary(),
      Response :: #{binary() => binary() | float()}.
do_call_users(Threshold) ->
    {ok, ConnPid} = gun:open(
                        ?DEFAULT_HOST,
                        application:get_env(?APPLICATION, port, ?DEFAULT_PORT)),
    StreamRef = gun:get(ConnPid, <<"/api/1/users?threshold=", Threshold/binary>>),
    ?assertMatch({response, nofin, 200, _Headers},
                  gun:await(ConnPid, StreamRef)),
    {ok, Body} = gun:await_body(ConnPid, StreamRef),
    ok = gun:shutdown(ConnPid),
    jiffy:decode(Body, [return_maps]).

%%------------------------------------------------------------------------------
%% @doc Call the users endpoint with a given user ID and a threshold and return
%%      the parsed result.
%% @end
%%------------------------------------------------------------------------------
-spec do_call_user(UserId, Threshold) -> Response when
      UserId :: xss_user:user_id(),
      Threshold :: binary(),
      Response :: #{binary() => binary() | float()}.
do_call_user(UserId, Threshold) ->
    {ok, ConnPid} = gun:open(
                        ?DEFAULT_HOST,
                        application:get_env(?APPLICATION, port, ?DEFAULT_PORT)),
    StreamRef = gun:get(ConnPid, <<"/api/1/users/",
                                   UserId/binary,
                                   "?threshold=",
                                   Threshold/binary>>),
    ?assertMatch({response, nofin, 200, _Headers},
                  gun:await(ConnPid, StreamRef)),
    {ok, Body} = gun:await_body(ConnPid, StreamRef),
    ok = gun:shutdown(ConnPid),
    jiffy:decode(Body, [return_maps]).

%%------------------------------------------------------------------------------
%% @doc Read the counter, then increment it's value by 1.
%% @end
%%------------------------------------------------------------------------------
-spec get_next_sequnce_number(Counter) -> integer() when
      Counter :: counters:counters_ref().
get_next_sequnce_number(Counter) ->
    SequenceNumber = counters:get(Counter, 1),
    ok = counters:add(Counter, 1, 1),
    SequenceNumber.

%%------------------------------------------------------------------------------
%% @doc Wait until the predicate function returns true or until timeout.
%% @end
%%------------------------------------------------------------------------------
-spec wait_until(Function, SleepPeriod, WaitTimeout) -> ok | timeout when
      Function :: fun(),
      SleepPeriod :: integer(),
      WaitTimeout :: integer().
wait_until(Function, SleepPeriod, WaitTimeout) ->
    StartTime = erlang:system_time(millisecond),
    do_wait_until(Function, SleepPeriod, WaitTimeout, StartTime).


%%------------------------------------------------------------------------------
%% @doc Helper function for `wait_until/3'
%% @end
%%------------------------------------------------------------------------------
-spec do_wait_until(Function, SleepPeriod, WaitTimeout, StartTime) -> ok | timeout when
      Function :: fun(),
      SleepPeriod :: integer(),
      WaitTimeout :: integer(),
      StartTime :: integer().
do_wait_until(Function, SleepPeriod, WaitTimeout, StartTime) ->
    case
        (erlang:system_time(millisecond) - StartTime) < WaitTimeout
    of
        true ->
            case
                Function()
            of
              true ->
                  ok;
              false ->
                  timer:sleep(SleepPeriod),
                  do_wait_until(Function, SleepPeriod, WaitTimeout, StartTime)
            end;
        false ->
            timeout
    end.

%%------------------------------------------------------------------------------
%% @doc Insert a verification to the db and update it's result.
%% @end
%%------------------------------------------------------------------------------
-spec insert_new_verification_to_db(map()) -> ok.
insert_new_verification_to_db(Config) ->
  VerificationId = maps:get(verification_id, Config, xss_utils:generate_uuid()),
  Result = maps:get(result, Config, ?VERIFICATION_RESULT),
  Verification = xss_verification:new(#{
      verification_id => VerificationId,
      profile_id => maps:get(profile_id, Config, xss_utils:generate_uuid()),
      stream_id => maps:get(stream_id, Config, xss_utils:generate_uuid()),
      last_chunk => 0,
      chunk_count => 0}),
  ?assertMatch({ok, 1}, xss_verification_store:insert_verification(Verification)),
  ?assertMatch({ok, 1},
               xss_verification_store:update_verification_success(VerificationId,
                                                                  Result)),
  ok.
