%%%-----------------------------------------------------------------------------
%%% @doc This module contains API functions required by the
%%%      x11-sentinel-dashboard application.
%%% @end
%%%-----------------------------------------------------------------------------

-module(xss_dashboard_api).
-include("x11_sentinel_server.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

-export([get_threshold/1,
         select_verifications_and_user_id_by_threshold/1,
         select_verifications_by_user_id_and_threshold/2,
         select_events_aggregated_by_day/0,
         select_events_by_user_id_aggregated_by_day/1]).

%%%=============================================================================
%%% Macros
%%%=============================================================================

%% Default verification threshold
%%
%% It is set to 1.0 which means that all verifications will be returned if the
%% client does not specify a threshold.
-define(DEFAULT_VERIFICATION_THRESHOLD, 1.0).


%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get all successful verifications where the score is lower than a
%%      threshold by the `user_id' field.
%% @end
%%------------------------------------------------------------------------------
-spec select_verifications_by_user_id_and_threshold(UserId, Threshold) -> Result when
      UserId :: xss_user:user_id(),
      Threshold :: float(),
      Result :: {ok, [#{binary() => any()}]}.
select_verifications_by_user_id_and_threshold(UserId, Threshold) ->
    case
      xss_database_server:execute(select_verifications_by_user_id_and_threshold,
                                  [UserId, Threshold])
    of
        {ok, _Columns, []} ->
            {ok, []};
        {ok, _Columns, Rows} ->
            {ok, [parse_db_query_result(select_verifications_by_user_id_and_threshold, Row) ||  Row <- Rows]}
    end.

%%------------------------------------------------------------------------------
%% @doc Get all successful verifications with user IDs where the score is lower
%%      than a threshold.
%% @end
%%------------------------------------------------------------------------------
-spec select_verifications_and_user_id_by_threshold(Threshold) -> Result when
      Threshold :: float(),
      Result :: {ok, [#{binary() => any()}]}.
select_verifications_and_user_id_by_threshold(Threshold) ->
    case
      xss_database_server:execute(select_verifications_and_user_id_by_threshold,
                                  [Threshold])
    of
        {ok, _Columns, []} ->
            {ok, []};
        {ok, _Columns, Rows} ->
            {ok, [parse_db_query_result(select_verifications_and_user_id_by_threshold, Row) ||  Row <- Rows]}
    end.

%%------------------------------------------------------------------------------
%% @doc Get daily aggregated event counts form all users.
%% @end
%%------------------------------------------------------------------------------
-spec select_events_aggregated_by_day() -> Result when
      Result :: {ok, [#{binary() => any()}]}.
select_events_aggregated_by_day() ->
    case
      xss_database_server:execute(select_events_aggregated_by_day, [])
    of
        {ok, _Columns, []} ->
            {ok, []};
        {ok, _Columns, Rows} ->
            {ok, [parse_db_query_result(select_events_aggregated_by_day, Row) ||  Row <- Rows]}
    end.

%%------------------------------------------------------------------------------
%% @doc Get daily aggregated event counts form a specific user.
%% @end
%%------------------------------------------------------------------------------
-spec select_events_by_user_id_aggregated_by_day(UserId) -> Result when
      UserId :: xss_user:user_id(),
      Result :: {ok, [#{binary() => any()}]}.
select_events_by_user_id_aggregated_by_day(UserId) ->
    case
      xss_database_server:execute(select_events_by_user_id_aggregated_by_day,
                                  [UserId])
    of
        {ok, _Columns, []} ->
            {ok, []};
        {ok, _Columns, Rows} ->
            {ok, [parse_db_query_result(select_events_by_user_id_aggregated_by_day, Row) ||  Row <- Rows]}
    end.

%%%-----------------------------------------------------------------------------
%%% @doc Match and parse the `threshold' from the request's query string or
%%%     return default.
%%% @end
%%%-----------------------------------------------------------------------------
-spec get_threshold(Request) -> Threshold when
      Request :: cowboy_reg:req(),
      Threshold :: float().
get_threshold(Request) ->
    #{threshold := Threshold} =
      cowboy_req:match_qs([{threshold,
                            [],
                            ?DEFAULT_VERIFICATION_THRESHOLD}],
                          Request),
    parse_binary_threshold_to_float(Threshold).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Try to parse a binary threshold to a float, return default if cannot.
%% @end
%%------------------------------------------------------------------------------
-spec parse_binary_threshold_to_float(binary()) -> float().
parse_binary_threshold_to_float(Threshold) when is_float(Threshold) ->
    Threshold;
parse_binary_threshold_to_float(Threshold) when is_binary(Threshold) ->
    String = binary_to_list(Threshold),
    case string:to_float(String) of
        {error, no_float} ->
            ok = logger:warning(#{reason => <<"Could not parse threshold, using default.">>,
                                  threshold => Threshold}),
            ?DEFAULT_VERIFICATION_THRESHOLD;
        {Float, _Rest} ->
            Float
    end.

%%-----------------------------------------------------------------------------
%% @doc Parse a raw database query result into the desired output.
%% @end
%%-----------------------------------------------------------------------------
-spec parse_db_query_result(QueryName, DbResult) -> Result when
      QueryName :: atom(),
      DbResult :: tuple(),
      Result :: #{atom() => any()}.
parse_db_query_result(select_verifications_and_user_id_by_threshold,
                      {VerificationId, Result, SucceededAt, UserId}) ->
    #{verification_id => VerificationId,
      result => xss_utils:null_to_undefined(Result),
      date => xss_utils:epgsql_timestamp_to_iso8601(SucceededAt),
      user_id => UserId};
parse_db_query_result(select_verifications_by_user_id_and_threshold,
                      {VerificationId, Result, SucceededAt, UserId}) ->
    #{verification_id => VerificationId,
      result => xss_utils:null_to_undefined(Result),
      date => xss_utils:epgsql_timestamp_to_iso8601(SucceededAt),
      user_id => UserId};
parse_db_query_result(select_events_aggregated_by_day,
                      {Date, EventCount}) ->
    #{date => xss_utils:epgsql_timestamp_to_iso8601(Date),
      event_count => EventCount};
parse_db_query_result(select_events_by_user_id_aggregated_by_day,
                      {Date, EventCount}) ->
    #{date => xss_utils:epgsql_timestamp_to_iso8601(Date),
      event_count => EventCount}.
