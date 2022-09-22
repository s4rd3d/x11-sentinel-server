%%%-----------------------------------------------------------------------------
%%% @doc The stream model represents a data stream of the `x11-sentinel-client'
%%%      application.
%%% @end
%%%-----------------------------------------------------------------------------

-module(xss_stream).
-include("x11_sentinel_server.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

-export([new/1,
         get_stream_id/1,
         get_session_id/1,
         get_user_id/1,
         get_created_at/1,
         get_updated_at/1]).

-export_type([stream/0,
              stream_id/0]).

%%%=============================================================================
%%% Types
%%%=============================================================================

%% Primary identifier of the stream.
-type stream_id() :: binary().

-type stream() :: #{stream_id := stream_id(),
                    % The session this stream belongs to.
                    session_id := xss_user:session_id(),
                    % The user this stream belongs to.
                    user_id := xss_user:user_id(),
                    % When was the stream added to the database.
                    created_at := xss_utils:xss_timestamp() | undefined,
                    % When was the stream last modified.
                    updated_at := xss_utils:xss_timestamp() | undefined}.

%%%=============================================================================
%%% Exported functions
%%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Create a new stream.
%% @end
%%-----------------------------------------------------------------------------
-spec new(#{stream_id := StreamId,
            session_id := SessionId,
            user_id := UserId,
            created_at => CreatedAt,
            updated_at => UpdatedAt}) -> Stream when
      StreamId :: stream_id(),
      SessionId :: xss_session:session_id(),
      UserId :: xss_user:user_id(),
      CreatedAt :: xss_utils:xss_timestamp() | undefined,
      UpdatedAt :: xss_utils:xss_timestamp() | undefined,
      Stream :: stream().
new(#{stream_id := StreamId,
      session_id := SessionId,
      user_id := UserId} = Stream) ->
    #{stream_id => StreamId,
      session_id => SessionId,
      user_id => UserId,
      created_at => maps:get(created_at, Stream, undefined),
      updated_at => maps:get(updated_at, Stream, undefined)}.

%%-----------------------------------------------------------------------------
%% @doc Get the `stream_id' field of the stream.
%% @end
%%-----------------------------------------------------------------------------
-spec get_stream_id(Stream) -> StreamId when
      Stream :: stream(),
      StreamId :: stream_id().
get_stream_id(#{stream_id := StreamId}) ->
    StreamId.

%%-----------------------------------------------------------------------------
%% @doc Get the `session_id' field of the stream.
%% @end
%%-----------------------------------------------------------------------------
-spec get_session_id(Stream) -> SessionId when
      Stream :: stream(),
      SessionId :: xss_session:session_id().
get_session_id(#{session_id := SessionId}) ->
    SessionId.

%%-----------------------------------------------------------------------------
%% @doc Get the `user_id' field of the stream.
%% @end
%%-----------------------------------------------------------------------------
-spec get_user_id(Stream) -> UserId when
      Stream :: stream(),
      UserId :: xss_user:user_id().
get_user_id(#{user_id := UserId}) ->
    UserId.

%%-----------------------------------------------------------------------------
%% @doc Get the `created_at' field of the stream.
%% @end
%%-----------------------------------------------------------------------------
-spec get_created_at(Stream) -> CreatedAt when
      Stream :: stream(),
      CreatedAt :: xss_utils:xss_timestamp() | undefined.
get_created_at(#{created_at := CreatedAt}) ->
    CreatedAt.

%%-----------------------------------------------------------------------------
%% @doc Get the `updated_at' field of the stream.
%% @end
%%-----------------------------------------------------------------------------
-spec get_updated_at(Stream) -> UpdatedAt when
      Stream :: stream(),
      UpdatedAt :: xss_utils:xss_timestamp() | undefined.
get_updated_at(#{updated_at := UpdatedAt}) ->
    UpdatedAt.
