%%%-----------------------------------------------------------------------------
%%% @doc Cowboy REST handler for handling status requests sent to
%%%      `api/[VERSION]/status/:user_id/:stream_id'.
%%% @end
%%%-----------------------------------------------------------------------------

-module(xss_status_rest_handler).
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
    UserId = cowboy_req:binding(user_id, Request, undefined),
    StreamId = cowboy_req:binding(stream_id, Request, undefined),

    case
        xss_user_store:select_user_by_user_id(UserId)
    of
        {ok, User} ->
            Status = get_status(User, StreamId),
            {Status, Request, State};
        {error, #{reason := user_not_found}} ->
            {<<"">>, Request, State}
    end.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get the status of the user and return it in JSON.
%% @end
%%------------------------------------------------------------------------------
-spec get_status(User, StreamId) -> Status when
      User :: xss_user:user(),
      StreamId :: xss_stream:stream_id(),
      Status :: iodata().
get_status(User, StreamId1) ->
    UserId = xss_user:get_user_id(User),

    case
        xss_verification_store:select_latest_succeeded_verification_by_user_id(UserId)
    of
        % The latest stream has at least one verification --> verify
        {ok, #{stream_id := StreamId2} = Verification} when StreamId1 =:= StreamId2 ->
            Result = xss_verification:get_result(Verification),
            jiffy:encode(#{<<"phase">> => <<"verify">>,
                           <<"description">> => <<"verification phase">>,
                           <<"value">> => Result});

        % The latest stream does not have at least one verification --> transition
        {ok, _Verification} ->
            Value = get_transition_value(StreamId1),
            jiffy:encode(#{<<"phase">> => <<"transition">>,
                           <<"description">> => <<"transition phase">>,
                           <<"value">> => Value});

        % The user does not have at least one verification --> learn
        {error, #{reason := verification_not_found}} ->
            Value = get_learn_value(User),
            jiffy:encode(#{<<"phase">> => <<"learn">>,
                           <<"description">> => <<"learning phase">>,
                           <<"value">> => Value})
    end.

%%------------------------------------------------------------------------------
%% @doc Calculate the value in learning phase.
%% @end
%%------------------------------------------------------------------------------
-spec get_learn_value(User) -> Value when
      User :: xss_user:user(),
      Value :: float().
get_learn_value(User) ->
    EventCount = xss_user:get_event_count(User),
    MinimumEventCount = xss_utils:minimum_event_count_for_profile(),
    min(EventCount / MinimumEventCount, 1).

%%------------------------------------------------------------------------------
%% @doc Calculate the value in transition phase.
%% @end
%%------------------------------------------------------------------------------
-spec get_transition_value(StreamId) -> Value when
      StreamId :: xss_stream:stream_id(),
      Value :: float().
get_transition_value(StreamId) ->
    {ok, EventCount} = xss_chunk_store:select_event_count_by_stream_id(StreamId),
    MinimumEventCount = xss_utils:minimum_event_count_for_verification(),
    min(EventCount / MinimumEventCount, 1).
