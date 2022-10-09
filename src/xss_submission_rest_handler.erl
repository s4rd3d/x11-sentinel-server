%%%-----------------------------------------------------------------------------
%%% @doc Cowboy REST handler for handling submission requests sent to
%%%      `api/[VERSION]/s'.
%%% @end
%%%-----------------------------------------------------------------------------

-module(xss_submission_rest_handler).
-include("x11_sentinel_server.hrl").
-behaviour(cowboy_rest).

%%%=============================================================================
%%% Exports
%%%=============================================================================

%% `cowboy_handler' callbacks
-export([init/2]).

%% `cowboy_rest' callbacks
-export([allowed_methods/2,
         content_types_accepted/2,
         valid_entity_length/2]).

%% Configured, custom callbacks to handle `cowboy_rest' calls
-export([accept_json/2]).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type state() :: #{}.

%%%=============================================================================
%%% Macros
%%%=============================================================================

%% Maximum allowed length of a request
-define(DEFAULT_REQUEST_MAX_LENGTH, (2*1024*1024)). % 2MB

%% Default referer value when the client does not send one
-define(DEFAULT_REFERER, <<"undefined">>).

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
    {[<<"POST">>], Request, State}.

%%------------------------------------------------------------------------------
%% @doc `content_type_accepted' callback.
%% @end
%%------------------------------------------------------------------------------
-spec content_types_accepted(Request, State) -> Result when
      Result :: {ContentTypes, Request, State},
      Request :: cowboy_req:req(),
      State :: state(),
      ContentTypes :: [{ParsedMime, AcceptCallback}],
      AcceptCallback :: atom(),
      ParsedMime :: {Type, SubType, '*'},
      Type :: binary(),
      SubType :: binary().
content_types_accepted(Request, State) ->
    {[{{<<"application">>, <<"json">>, '*'}, accept_json}], Request, State}.

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
%% @doc `AcceptCallback' handler for JSON requests.
%% @end
%%------------------------------------------------------------------------------
-spec accept_json(Request, State) -> {true | false, Request, State} when
      Request :: cowboy_req:req(),
      State :: state().
accept_json(Request, State) ->
    do_accept_object(Request, State).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Handle common `AcceptCallback' parts.
%% @end
%%------------------------------------------------------------------------------
%%
-spec do_accept_object(Request, State) ->
    {Result, Request, State} when
      Request :: cowboy_req:req(),
      Result :: boolean(),
      State :: state().
do_accept_object(Request1, State) ->
    % Get the connection's properties
    {RealIpAddress, PeerIpAddress} = get_connection_properties(Request1),

    % Extract the HTTP referer
    Referer = cowboy_req:header(<<"referer">>, Request1, ?DEFAULT_REFERER),

    % 1. Read request
    {ok, Body, Request2} = read_body(Request1),

    % 2. Handle the request body
    ParsedBody = #{} = parse_body(Body),

    % 3. Send submitted data to storage
    Chunk = xss_chunk:new(ParsedBody, RealIpAddress, PeerIpAddress, Referer),

    % 4. Maybe add new user, session and stream to the database
    UserId = xss_chunk:get_user_id(Chunk),
    SessionId = xss_chunk:get_session_id(Chunk),
    StreamId = xss_chunk:get_stream_id(Chunk),
    ok = maybe_add_user(UserId),
    ok = maybe_add_session(SessionId),
    ok = maybe_add_stream(StreamId, SessionId, UserId),

    % 5. Update event count of user
    ok = increase_event_count(UserId, xss_chunk:calculate_event_count(Chunk)),

    % 6. Persist chunk.
    {ok, _RowsEffected} = xss_chunk_store:insert_chunk(Chunk),

    ok = logger:info(#{message => <<"New chunk saved to the database">>,
                       user_id => UserId,
                       session_id => xss_chunk:get_session_id(Chunk),
                       stream_id => xss_chunk:get_stream_id(Chunk),
                       sequence_number => xss_chunk:get_sequence_number(Chunk)}),

    % 7. Call asynchronous after persist callback function.
    ok = xss_api_server:after_persist(UserId, Chunk),

    % 8. Compile and return results.
    Response = jiffy:encode(#{<<"response">> => <<"ok">>}),
    ResponseHeaders = [{<<"access-control-allow-methods">>, <<"POST">>},
                       {<<"access-control-allow-headers">>,
                        <<"Origin, X-Requested-With, Content-Type, Accept">>},
                       {<<"access-control-allow-origin">>, <<"*">>}],
    Request3 = lists:foldl(fun({K, V}, Req) ->
                                   cowboy_req:set_resp_header(K, V, Req)
                           end,
                           Request2,
                           ResponseHeaders),
    Result = cowboy_req:set_resp_body(Response, Request3),
    {true, Result, State}.

%%------------------------------------------------------------------------------
%% @doc Extract IP address and peer IP address from a `cowboy_req:req()' object.
%% @end
%%------------------------------------------------------------------------------
-spec get_connection_properties(Request) -> Properties when
      Request :: cowboy_req:req(),
      Properties :: {RealIpAddress, PeerIpAddress},
      RealIpAddress :: inet:ip_address(),
      PeerIpAddress :: inet:ip_address().
get_connection_properties(Request) ->
    {PeerIpAddress, _Port} = cowboy_req:peer(Request),

    % Extract real client IP address from `X-Forwarded-For' HTTP header
    XForwardedFor = cowboy_req:header(<<"x-forwarded-for">>,
                                      Request,
                                      <<"undefined">>),
    [BinaryRealIpAddress | _Rest] = string:split(XForwardedFor, ","),
    RealIpAddress = do_parse_binary_ip_address(BinaryRealIpAddress,
                                               PeerIpAddress),

    {RealIpAddress, PeerIpAddress}.

%%------------------------------------------------------------------------------
%% @doc Parse a binary IP address to `inet:ip_address'. Return a default value
%%      if the binary IP is malformed or missing.
%% @end
%%------------------------------------------------------------------------------
-spec do_parse_binary_ip_address(Binary, Default) -> Result when
      Binary :: binary(),
      Default :: inet:ip_address(),
      Result :: inet:ip_address().
do_parse_binary_ip_address(<<"undefined">>, Default) ->
    Default;
do_parse_binary_ip_address(Binary, Default) when is_binary(Binary) ->
    case
        inet:parse_address(binary_to_list(Binary))
    of
        {ok, IpAddress} ->
            IpAddress;
        {error, Reason} ->
            logger:warning(#{message => "Could not parse client IP address",
                             reason => Reason,
                             binary_ip_address => Binary}),
            Default
    end.

%%------------------------------------------------------------------------------
%% @doc Read the whole body of a request.
%% @end
%%------------------------------------------------------------------------------
-spec read_body(Request) -> {ok, Body, Request} when
      Request :: cowboy_req:req(),
      Body :: binary().
read_body(Request) ->
    read_body(Request, <<>>).

%%------------------------------------------------------------------------------
%% @doc Read the whole body of a request.
%%
%%      This function is proposed in the documentation of
%%      `cowboy_req:read_body(3)'.
%% @end
%%------------------------------------------------------------------------------
-spec read_body(Request, Acc) -> {ok, Acc, Request} when
      Request :: cowboy_req:req(),
      Acc :: binary().
read_body(Request0, Acc) ->
    case cowboy_req:read_body(Request0) of
        {ok, Data, Request1} ->
            {ok, <<Acc/binary, Data/binary>>, Request1};
        {more, Data, Request1} ->
            read_body(Request1, <<Acc/binary, Data/binary>>)
    end.

%%------------------------------------------------------------------------------
%% @doc Parse a JSON HTTP request body into Erlang terms.
%%      Also transform camelCase keys to snake_case.
%% @end
%%------------------------------------------------------------------------------
-spec parse_body(JsonBody) -> ParsedBody when
      JsonBody :: binary(),
      ParsedBody :: #{atom() => binary()}.
parse_body(JsonBody) ->
  Map = jiffy:decode(JsonBody, [return_maps, copy_strings]),
  xss_utils:camel_to_snake(Map).

%%------------------------------------------------------------------------------
%% @doc Maybe add a new user to the database.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_user(UserId) -> ok when
      UserId :: xss_user:user_id().
maybe_add_user(UserId) ->
    case
        xss_user_store:select_user_by_user_id(UserId)
    of
        {ok, _User} ->
          ok;
        {error, #{reason := user_not_found}} ->
          ok = logger:info(#{message => <<"Adding new user">>,
                             user_id => UserId}),
          User = xss_user:new(#{user_id => UserId}),
          {ok, _RowsEffected} = xss_user_store:insert_user(User),
          ok
    end.

%%------------------------------------------------------------------------------
%% @doc Increase the event count of a user.
%% @end
%%------------------------------------------------------------------------------
-spec increase_event_count(UserId, EventCount) -> ok when
      UserId :: xss_user:user_id(),
      EventCount :: non_neg_integer().
increase_event_count(UserId, EventCount) ->
    {ok, User} = xss_user_store:select_user_by_user_id(UserId),
    OldEventCount = xss_user:get_event_count(User),
    NewEventCount = OldEventCount + EventCount,
    {ok, _RowsEffected} =
        xss_user_store:update_user_event_count(User, NewEventCount),
    ok.

%%------------------------------------------------------------------------------
%% @doc Maybe add a new session to the database.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_session(SessionId) -> ok when
      SessionId :: xss_session:session_id().
maybe_add_session(SessionId) ->
    case
        xss_session_store:select_session_by_session_id(SessionId)
    of
        {ok, _Session} ->
          ok;
        {error, #{reason := session_not_found}} ->
          ok = logger:info(#{message => <<"Adding new session">>,
                             session_id => SessionId}),
          Session = xss_session:new(#{session_id => SessionId}),
          {ok, _RowsEffected} = xss_session_store:insert_session(Session),
          ok
    end.

%%------------------------------------------------------------------------------
%% @doc Maybe add a new stream to the database.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_stream(StreamId, SessionId, UserId) -> ok when
      StreamId :: xss_stream:stream_id(),
      SessionId :: xss_session:session_id(),
      UserId :: xss_user:user_id().
maybe_add_stream(StreamId, SessionId, UserId) ->
    case
        xss_stream_store:select_stream_by_stream_id(StreamId)
    of
        {ok, _Stream} ->
          ok;
        {error, #{reason := stream_not_found}} ->
          ok = logger:info(#{message => <<"Adding new stream">>,
                             stream_id => StreamId,
                             session_id => SessionId,
                             user_id => UserId}),
          Stream = xss_stream:new(#{stream_id => StreamId,
                                    session_id => SessionId}),
          {ok, _RowsEffected} = xss_stream_store:insert_stream(Stream),
          ok
    end.
