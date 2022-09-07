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
    [http_connectivity_test].

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

%%%=============================================================================
%%% Helper functions
%%%=============================================================================
