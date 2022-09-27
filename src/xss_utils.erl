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
         camel_to_snake/1,
         snake_to_camel/1]).

-export_type([xss_timestamp/0,
              epgsql_timestamp/0]).

%%%=============================================================================
%%% Macros
%%%=============================================================================

% calendar:datetime_to_gregorian_seconds({{1970, 1, 1},{0, 0, 0}})
-define(GREGORIAN_1970, 62167219200).

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
key_camel_to_snake(<<"sequenceNumber">>) ->
    sequence_number;
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
key_snake_to_camel(sequence_number) ->
    <<"sequenceNumber">>;
key_snake_to_camel(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom);
key_snake_to_camel(Key) ->
    Key.
