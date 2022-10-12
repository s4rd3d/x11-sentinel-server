%%%-----------------------------------------------------------------------------
%%% @doc Cowboy REST handler for handling status requests sent to
%%%      `api/[VERSION]/users'.
%%% @end
%%%-----------------------------------------------------------------------------

-module(xss_users_rest_handler).
-include("x11_sentinel_server.hrl").
-behaviour(cowboy_rest).

%%%=============================================================================
%%% Exports
%%%=============================================================================

%% `cowboy_handler' callbacks
-export([init/2]).

%% `cowboy_rest' callbacks
-export([allowed_methods/2,
         content_types_provided/2,
         valid_entity_length/2]).

%% Configured, custom callbacks to handle `cowboy_rest' calls
-export([provide_json/2]).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type state() :: #{}.

%%%=============================================================================
%%% Macros
%%%=============================================================================

%% Maximum allowed length of a request
-define(DEFAULT_REQUEST_MAX_LENGTH, (2*1024)). % 2KB

%%%=============================================================================
%%% Exported functions
%%%=============================================================================

%%%-----------------------------------------------------------------------------
%%% `cowboy_handler' callbacks
%%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Initialize handler.
%% @end
%%------------------------------------------------------------------------------
-spec init(Request, State) -> {cowboy_rest, Request, State} when
      Request :: cowboy_req:req(),
      State :: state().
init(Request, #{} = State) ->
    {cowboy_rest, Request, State}.

%%%-----------------------------------------------------------------------------
%%% `cowboy_rest' callbacks
%%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc `allowed_methods' callback.
%% @end
%%------------------------------------------------------------------------------
-spec allowed_methods(Request, State) -> {Methods, Request, State} when
      Request :: cowboy_req:req(),
      State :: state(),
      Methods :: [binary()].
allowed_methods(Request, State) ->
    {[<<"GET">>], Request, State}.

%%------------------------------------------------------------------------------
%% @doc `content_types_provided' callback.
%% @end
%%------------------------------------------------------------------------------
-spec content_types_provided(Request, State) -> Result when
      Request :: cowboy_req:req(),
      State :: state(),
      Result :: {[{ContentType, Callback}], Request, State},
      ContentType :: binary(),
      Callback :: atom().
content_types_provided(Request, State) ->
    {[{<<"application/json">>, provide_json}], Request, State}.

%%------------------------------------------------------------------------------
%% @doc `valid_entity_length' callback.
%% @end
%%------------------------------------------------------------------------------
-spec valid_entity_length(Request, State) -> {Result, Request, State} when
      Request :: cowboy_req:req(),
      State :: state(),
      Result :: boolean().
valid_entity_length(Request, State) ->
    MaxLength = application:get_env(?APPLICATION,
                                    request_max_length,
                                    ?DEFAULT_REQUEST_MAX_LENGTH),
    case
        cowboy_req:body_length(Request)
    of
        Length when Length < MaxLength ->
            {true, Request, State};
        _ ->
            {false, Request, State}
    end.

%%%-----------------------------------------------------------------------------
%%% Configured custom handler
%%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc `ProvideCallback' handler for JSON requests.
%% @end
%%------------------------------------------------------------------------------
-spec provide_json(Request, State) -> {ResponseBody, Request, State} when
      Request :: cowboy_req:req(),
      State :: state(),
      ResponseBody :: iodata().
provide_json(Request, State) ->
    case
        cowboy_req:binding(user_id, Request, undefined)
    of
        undefined ->
            provide_users_json(Request, State);
        UserId ->
            provide_user_json(UserId, Request, State)
    end.

-spec provide_users_json(Request, State) -> {ResponseBody, Request, State} when
      Request :: cowboy_req:req(),
      State :: state(),
      ResponseBody :: iodata().
provide_users_json(Request1, State) ->
    % Select all users
    {ok, Users1} = xss_user_store:select_users(),

    Threshold = xss_dashboard_api:get_threshold(Request1),

    % Construct the result
    Users2 = lists:map(
      fun (#{user_id := UserId,
             event_count := EventCount,
             created_at := CreatedAt} = _User) ->
          {ok, Verifications} =
            xss_dashboard_api:select_verifications_by_user_id_and_threshold(UserId, 1.0),
          {ok, Incidents} =
            xss_dashboard_api:select_verifications_by_user_id_and_threshold(UserId, Threshold),
          #{<<"userId">> => UserId,
            <<"eventCount">> => EventCount,
            <<"createdAt">> => xss_utils:xss_timestamp_to_iso8601(CreatedAt),
            <<"verifications">> => length(Verifications),
            <<"incidents">> => length(Incidents)}
      end,
      Users1),

    % Set response headers
    Request2 = xss_utils:add_cors_headers(Request1),

    {jiffy:encode(#{result => ok, users => Users2}), Request2, State}.

-spec provide_user_json(UserId, Request, State) -> {ResponseBody, Request, State} when
      UserId :: xss_user:user_id(),
      Request :: cowboy_req:req(),
      State :: state(),
      ResponseBody :: iodata().
provide_user_json(UserId, Request1, State) ->
    {ok, User1} = xss_user_store:select_user_by_user_id(UserId),

    Threshold = xss_dashboard_api:get_threshold(Request1),

    {ok, Verifications} =
      xss_dashboard_api:select_verifications_by_user_id_and_threshold(UserId, 1.0),
    {ok, Incidents} =
      xss_dashboard_api:select_verifications_by_user_id_and_threshold(UserId, Threshold),

    User2 = #{<<"userId">> => xss_user:get_user_id(User1),
              <<"eventCount">> => xss_user:get_event_count(User1),
              <<"createdAt">> => xss_utils:xss_timestamp_to_iso8601(xss_user:get_created_at(User1)),
              <<"verifications">> => length(Verifications),
              <<"incidents">> => length(Incidents)},

    % Set response headers
    Request2 = xss_utils:add_cors_headers(Request1),

    {jiffy:encode(#{result => ok, user => User2}), Request2, State}.
