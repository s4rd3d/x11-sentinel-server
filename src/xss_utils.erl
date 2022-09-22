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
         epgsql_timestamp_to_xss_timestamp/1]).

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

