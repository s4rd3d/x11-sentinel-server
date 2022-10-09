%%%-----------------------------------------------------------------------------
%%% @doc This module contains API functions to execute queries on the database
%%%      related to the {@link xss_stream} model.
%%% @end
%%%-----------------------------------------------------------------------------

-module(xss_stream_store).
-include("x11_sentinel_server.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

-export([select_stream_by_stream_id/1,
         select_latest_stream_by_user_id/1,
         insert_stream/1,
         soft_delete_stream_by_stream_id/1]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get a stream from the database by the `stream_id' field.
%% @end
%%------------------------------------------------------------------------------
-spec select_stream_by_stream_id(StreamId) -> Result when
      StreamId :: xss_stream:stream_id(),
      Result :: {ok, xss_stream:stream()} | {error, Reason},
      Reason :: any().
select_stream_by_stream_id(StreamId) ->
    case
      xss_database_server:execute(select_stream_by_stream_id, [StreamId])
    of
        {ok, _Columns, []} ->
            {error, #{reason => stream_not_found,
                      stream_id => StreamId}};
        {ok, _Columns, [Row | _Rest]} ->
            {ok, parse_db_row(Row)}
    end.

%%------------------------------------------------------------------------------
%% @doc Get the latest stream from the database by the `user_id' field.
%% @end
%%------------------------------------------------------------------------------
-spec select_latest_stream_by_user_id(UserId) -> Result when
      UserId :: xss_user:user_id(),
      Result :: {ok, xss_stream:stream()} | {error, Reason},
      Reason :: any().
select_latest_stream_by_user_id(UserId) ->
    case
      xss_database_server:execute(select_latest_stream_by_user_id, [UserId])
    of
        {ok, _Columns, []} ->
            {error, #{reason => stream_not_found,
                      user_id => UserId}};
        {ok, _Columns, [Row | _Rest]} ->
            {ok, parse_db_row(Row)}
    end.

%%------------------------------------------------------------------------------
%% @doc Insert a new stream into the database.
%% @end
%%------------------------------------------------------------------------------
-spec insert_stream(Stream) -> Result when
      Stream :: xss_stream:stream(),
      Result :: {ok, RowsEffected} | {error, Reason},
      RowsEffected :: integer(),
      Reason :: any().
insert_stream(#{stream_id := StreamId,
                session_id := SessionId,
                created_at := undefined,
                updated_at := undefined}) ->
    Now = xss_utils:xss_timestamp_to_epgsql_timestamp(xss_utils:now()),
    xss_database_server:execute(insert_stream,
                                [StreamId, SessionId, Now, Now]).

%%------------------------------------------------------------------------------
%% @doc Soft delete stream by setting the `deleted_at' field.
%% @end
%%------------------------------------------------------------------------------
-spec soft_delete_stream_by_stream_id(StreamId) -> Result when
      StreamId :: xss_stream:stream_id(),
      Result :: {ok, RowsEffected} | {error, Reason},
      RowsEffected :: integer(),
      Reason :: any().
soft_delete_stream_by_stream_id(StreamId) ->
    Now = xss_utils:xss_timestamp_to_epgsql_timestamp(xss_utils:now()),
    xss_database_server:execute(soft_delete_stream_by_stream_id,
                                [Now, Now, StreamId]).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Parse a raw database entry into a {@link xss_stream} model entity.
%% @end
%%-----------------------------------------------------------------------------
-spec parse_db_row({StreamId,
                    SessionId,
                    CreatedAt,
                    UpdatedAt}) -> Stream when
      StreamId :: xss_stream:stream_id(),
      SessionId :: xss_session:session_id(),
      CreatedAt :: xss_utils:epgsql_timestamp() | null,
      UpdatedAt :: xss_utils:epgsql_timestamp() | null,
      Stream :: xss_stream:stream().
parse_db_row({StreamId, SessionId, CreatedAt, UpdatedAt}) ->
      xss_stream:new(#{stream_id => StreamId,
                       session_id => SessionId,
                       created_at =>
                        xss_utils:epgsql_timestamp_to_xss_timestamp(CreatedAt),
                       updated_at =>
                        xss_utils:epgsql_timestamp_to_xss_timestamp(UpdatedAt)}).
