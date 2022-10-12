%%%-----------------------------------------------------------------------------
%%% @doc This module contains utility functions used by other modules.
%%% @end
%%%-----------------------------------------------------------------------------

-module(xss_utils).
-include("x11_sentinel_server.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

-export([now/0,
         xss_timestamp_to_epgsql_timestamp/1,
         epgsql_timestamp_to_xss_timestamp/1,
         epgsql_timestamp_to_iso8601/1,
         xss_timestamp_to_iso8601/1,
         camel_to_snake/1,
         snake_to_camel/1,
         null_to_undefined/1,
         generate_uuid/0,
         add_cors_headers/1,
         minimum_event_count_for_profile/0,
         minimum_event_count_for_verification/0,
         minimum_elapsed_time_for_failed_profile_rebuild/0,
         evaluation_service_host/0,
         evaluation_service_port/0,
         default_verification_threshold/0]).

-export_type([xss_timestamp/0,
              epgsql_timestamp/0]).

%%%=============================================================================
%%% Macros
%%%=============================================================================

% calendar:datetime_to_gregorian_seconds({{1970, 1, 1},{0, 0, 0}})
-define(GREGORIAN_1970, 62167219200).

%% Minimum event count for profile building
-define(DEFAULT_MINIMUM_EVENT_COUNT_FOR_PROFILE, 1000000). % 1_000_000

%% Minimum event count for verification
-define(DEFAULT_MINIMUM_EVENT_COUNT_FOR_VERIFICATION, 720).

%% Minimum time in milliseconds after a failed profile can be rebuilt (1 hour)
-define(DEFAULT_MINIMUM_ELAPSED_TIME_FOR_FAILED_PROFILE_REBUILD, 3600000).

%% Host of the evaluation service
-define(DEFAULT_EVALUATION_SERVICE_HOST, "localhost").

%% Evaluation service port
-define(DEFAULT_EVALUATION_SERVICE_PORT, 8081).

%% Default verification threshold
%%
%% It is set to 1.0 which means that all verifications will be returned if the
%% client does not specify a threshold.
-define(DEFAULT_VERIFICATION_THRESHOLD, 1.0).

%%%=============================================================================
%%% Types
%%%=============================================================================

% Microseconds.
-type xss_timestamp() :: integer().

-type year() :: non_neg_integer().
-type month() :: 1..12.
-type day() :: 1..31.
-type hour() :: 0..23.
-type minute() :: 0..59.
-type second_and_microsecond() :: float().

-type date() :: {year(), month(), day()}.
-type time() :: {hour(), minute(), second_and_microsecond()}.

-type epgsql_timestamp() :: {date(), time()}.

%%%=============================================================================
%%% Exported functions
%%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Unix time in microseconds.
%% @end
%%-----------------------------------------------------------------------------
-spec now() -> xss_timestamp().
now() ->
    erlang:system_time(microsecond).

%%-----------------------------------------------------------------------------
%% @doc Convert from xss timestamp to epgsql timestamp.
%% @end
%%-----------------------------------------------------------------------------
-spec xss_timestamp_to_epgsql_timestamp(Timestamp) -> Result when
      Timestamp :: xss_timestamp(),
      Result :: epgsql_timestamp() | null.
xss_timestamp_to_epgsql_timestamp(undefined) ->
    null;
xss_timestamp_to_epgsql_timestamp(Timestamp) ->
    {{Y, D, M}, {H, Mi, S}} =
     calendar:gregorian_seconds_to_datetime(?GREGORIAN_1970 +
      erlang:convert_time_unit(Timestamp, microsecond, second)),
    {{Y, D, M}, {H, Mi, S + (Timestamp rem 1000000) / 1000000}}.

%%-----------------------------------------------------------------------------
%% @doc Convert from epgsql timestamp to xss timestamp.
%% @end
%%-----------------------------------------------------------------------------
-spec epgsql_timestamp_to_xss_timestamp(Timestamp) -> Result when
      Timestamp :: epgsql_timestamp(),
      Result :: xss_timestamp() | undefined.
epgsql_timestamp_to_xss_timestamp(null) ->
    undefined;
epgsql_timestamp_to_xss_timestamp({{Y, M, D}, {H, Mi, S}}) ->
    Seconds = erlang:trunc(S),
    Microseconds = erlang:trunc((S - Seconds) * 1000000),
    GregorianSeconds = erlang:convert_time_unit(
        calendar:datetime_to_gregorian_seconds({{Y, M, D}, {H, Mi, Seconds}}) - ?GREGORIAN_1970,
        second,
        microsecond),
    GregorianSeconds + Microseconds.

%%-----------------------------------------------------------------------------
%% @doc Convert from epgsql timestamp to iso8601.
%% @end
%%-----------------------------------------------------------------------------
-spec epgsql_timestamp_to_iso8601(Timestamp) -> Result when
      Timestamp :: epgsql_timestamp(),
      Result :: binary() | undefined.
epgsql_timestamp_to_iso8601(null) ->
    undefined;
epgsql_timestamp_to_iso8601(Timestamp) ->
    iso8601:format(Timestamp).

%%-----------------------------------------------------------------------------
%% @doc Convert from xss timestamp to iso8601.
%% @end
%%-----------------------------------------------------------------------------
-spec xss_timestamp_to_iso8601(Timestamp) -> Result when
      Timestamp :: xss_timestamp(),
      Result :: binary() | undefined.
xss_timestamp_to_iso8601(Timestamp) ->
  epgsql_timestamp_to_iso8601(xss_timestamp_to_epgsql_timestamp(Timestamp)).

%%------------------------------------------------------------------------------
%% @doc Transform camelCase keys in a map recursively to snake_case.
%% @end
%%------------------------------------------------------------------------------
-spec camel_to_snake(Input) -> Result when
      Input :: #{binary() => any()} | #{atom() => any()},
      Result :: #{atom() => any()}.
camel_to_snake(#{} = Map) ->
    maps:fold(fun fold_camel_to_snake/3, #{}, Map);
camel_to_snake(Other) ->
    Other.

%%------------------------------------------------------------------------------
%% @doc Helper method for `camel_to_snake/1'.
%% @end
%%------------------------------------------------------------------------------
fold_camel_to_snake(Key, Value, Acc) ->
    maps:merge(Acc, #{key_camel_to_snake(Key) => camel_to_snake(Value)}).

%%------------------------------------------------------------------------------
%% @doc Map known camelCase keys to snake_case.
%% @end
%%------------------------------------------------------------------------------
key_camel_to_snake(<<"streamId">>) ->
    stream_id;
key_camel_to_snake(<<"userId">>) ->
    user_id;
key_camel_to_snake(<<"sessionId">>) ->
    session_id;
key_camel_to_snake(<<"verificationId">>) ->
    verification_id;
key_camel_to_snake(<<"profileId">>) ->
    profile_id;
key_camel_to_snake(<<"sequenceNumber">>) ->
    sequence_number;
key_camel_to_snake(<<"profileData">>) ->
    profile_data;
key_camel_to_snake(<<"lastChunk">>) ->
    last_chunk;
key_camel_to_snake(<<"chunkCount">>) ->
    chunk_count;
key_camel_to_snake(<<"eventCount">>) ->
    event_count;
key_camel_to_snake(<<"createddAt">>) ->
    created_at;
key_camel_to_snake(<<"succeededAt">>) ->
    succeeded_at;
key_camel_to_snake(<<"faileddAt">>) ->
    failed_at;
key_camel_to_snake(<<"updatedAt">>) ->
    updated_at;
key_camel_to_snake(Binary) when is_binary(Binary) ->
    binary_to_atom(Binary, utf8);
key_camel_to_snake(Key) ->
    Key.

%%------------------------------------------------------------------------------
%% @doc Transform snake_case keys in a map recursively to camelCase.
%% @end
%%------------------------------------------------------------------------------
-spec snake_to_camel(Input) -> Result when
      Input ::  #{atom() => any()},
      Result :: #{binary() => any()}.
snake_to_camel(#{} = Map) ->
    maps:fold(fun fold_snake_to_camel/3, #{}, Map);
snake_to_camel(Other) ->
    Other.

%%------------------------------------------------------------------------------
%% @doc Helper method for `snake_to_camel/1'.
%% @end
%%------------------------------------------------------------------------------
fold_snake_to_camel(Key, Value, Acc) ->
    maps:merge(Acc, #{key_snake_to_camel(Key) => snake_to_camel(Value)}).

%%------------------------------------------------------------------------------
%% @doc Map known camelCase keys to snake_case.
%% @end
%%------------------------------------------------------------------------------
key_snake_to_camel(stream_id) ->
    <<"streamId">>;
key_snake_to_camel(user_id) ->
    <<"userId">>;
key_snake_to_camel(session_id) ->
    <<"sessionId">>;
key_snake_to_camel(verification_id) ->
    <<"verificationId">>;
key_snake_to_camel(profile_id) ->
    <<"profileId">>;
key_snake_to_camel(sequence_number) ->
    <<"sequenceNumber">>;
key_snake_to_camel(profile_data) ->
    <<"profileData">>;
key_snake_to_camel(last_chunk) ->
    <<"lastChunk">>;
key_snake_to_camel(chunk_count) ->
    <<"chunkCount">>;
key_snake_to_camel(event_count) ->
    <<"eventCount">>;
key_snake_to_camel(created_at) ->
    <<"createddAt">>;
key_snake_to_camel(succeeded_at) ->
    <<"succeededAt">>;
key_snake_to_camel(failed_at) ->
    <<"faileddAt">>;
key_snake_to_camel(updated_at) ->
    <<"updatedAt">>;
key_snake_to_camel(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom);
key_snake_to_camel(Key) ->
    Key.

%%------------------------------------------------------------------------------
%% @doc Map database `null' value to Erlang type `undefined'.
%% @end
%%------------------------------------------------------------------------------
-spec null_to_undefined(Value) -> Value | undefined when
      Value :: any().
null_to_undefined(null) ->
    undefined;
null_to_undefined(Value) ->
    Value.

%%------------------------------------------------------------------------------
%% @doc Generate a unique identifier.
%% @end
%%------------------------------------------------------------------------------
-spec generate_uuid() -> Uuid when
      Uuid :: binary().
generate_uuid() ->
    list_to_binary(uuid:to_string(uuid:uuid4())).

-spec add_cors_headers(Request) -> Request when
      Request :: cowboy_req:req().
add_cors_headers(Request) ->
    Headers = [{<<"access-control-allow-headers">>,
                <<"Origin, X-Requested-With, Content-Type, Accept">>},
               {<<"access-control-allow-origin">>, <<"*">>}],
    lists:foldl(fun({K, V}, Req) ->
                        cowboy_req:set_resp_header(K, V, Req)
                end,
                Request,
                Headers).

%%------------------------------------------------------------------------------
%% @doc Return the minimum event count needed for profile building.
%% @end
%%------------------------------------------------------------------------------
-spec minimum_event_count_for_profile() -> EventCount when
      EventCount :: non_neg_integer().
minimum_event_count_for_profile() ->
    application:get_env(?APPLICATION,
                        minimum_event_count_for_profile,
                        ?DEFAULT_MINIMUM_EVENT_COUNT_FOR_PROFILE).

%%------------------------------------------------------------------------------
%% @doc Return the minimum event count needed for verification.
%% @end
%%------------------------------------------------------------------------------
-spec minimum_event_count_for_verification() -> EventCount when
      EventCount :: non_neg_integer().
minimum_event_count_for_verification() ->
    application:get_env(?APPLICATION,
                        minimum_event_count_for_verification,
                        ?DEFAULT_MINIMUM_EVENT_COUNT_FOR_VERIFICATION).

%%------------------------------------------------------------------------------
%% @doc Return the minimum time for profile rebuilding.
%% @end
%%------------------------------------------------------------------------------
-spec minimum_elapsed_time_for_failed_profile_rebuild() -> Time when
      Time :: integer().
minimum_elapsed_time_for_failed_profile_rebuild() ->
    application:get_env(?APPLICATION,
                        minimum_elapsed_time_for_failed_profile_rebuild,
                        ?DEFAULT_MINIMUM_ELAPSED_TIME_FOR_FAILED_PROFILE_REBUILD).

%%------------------------------------------------------------------------------
%% @doc Return the evaluation service host.
%% @end
%%------------------------------------------------------------------------------
-spec evaluation_service_host() -> Host when
      Host :: string().
evaluation_service_host() ->
    application:get_env(?APPLICATION,
                        evaluation_service_host,
                        ?DEFAULT_EVALUATION_SERVICE_HOST).

%%------------------------------------------------------------------------------
%% @doc Return the evaluation service port.
%% @end
%%------------------------------------------------------------------------------
-spec evaluation_service_port() -> Port when
      Port :: integer().
evaluation_service_port() ->
    application:get_env(?APPLICATION,
                        evaluation_service_port,
                        ?DEFAULT_EVALUATION_SERVICE_PORT).

%%------------------------------------------------------------------------------
%% @doc Return the default verification threshold.
%% @end
%%------------------------------------------------------------------------------
-spec default_verification_threshold() -> Threshold when
      Threshold :: float().
default_verification_threshold() ->
    ?DEFAULT_VERIFICATION_THRESHOLD.
