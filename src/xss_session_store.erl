%%%-----------------------------------------------------------------------------
%%% @doc This module contains API functions to execute queries on the database
%%%      related to the {@link xss_session} model.
%%% @end
%%%-----------------------------------------------------------------------------

-module(xss_session_store).
-include("x11_sentinel_server.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

-export([select_session_by_session_id/1,
         insert_session/1,
         soft_delete_session_by_session_id/1]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get a session from the database by the `session_id' field.
%% @end
%%------------------------------------------------------------------------------
-spec select_session_by_session_id(SessionId) -> Result when
      SessionId :: xss_session:session_id(),
      Result :: {ok, xss_session:session()} | {error, Reason},
      Reason :: any().
select_session_by_session_id(SessionId) ->
    case
      xss_database_server:execute(select_session_by_session_id, [SessionId])
    of
        {ok, _Columns, []} ->
            {error, #{reason => session_not_found,
                      session_id => SessionId}};
        {ok, _Columns, [Row | _Rest]} ->
            {ok, parse_db_row(Row)}
    end.

%%------------------------------------------------------------------------------
%% @doc Insert a new session into the database.
%% @end
%%------------------------------------------------------------------------------
-spec insert_session(Session) -> Result when
      Session :: xss_session:session(),
      Result :: {ok, RowsEffected} | {error, Reason},
      RowsEffected :: integer(),
      Reason :: any().
insert_session(#{session_id := SessionId,
                 created_at := undefined,
                 updated_at := undefined
            }) ->
    Now = xss_utils:xss_timestamp_to_epgsql_timestamp(xss_utils:now()),
    xss_database_server:execute(insert_session, [SessionId, Now, Now]).

%%------------------------------------------------------------------------------
%% @doc Soft delete session by setting the `deleted_at' field.
%% @end
%%------------------------------------------------------------------------------
-spec soft_delete_session_by_session_id(SessionId) -> Result when
      SessionId :: xss_session:session_id(),
      Result :: {ok, RowsEffected} | {error, Reason},
      RowsEffected :: integer(),
      Reason :: any().
soft_delete_session_by_session_id(SessionId) ->
    Now = xss_utils:xss_timestamp_to_epgsql_timestamp(xss_utils:now()),
    xss_database_server:execute(soft_delete_session_by_session_id,
                                [Now, Now, SessionId]).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Parse a raw database entry into a {@link xss_session} model entity.
%% @end
%%-----------------------------------------------------------------------------
-spec parse_db_row({SessionId, CreatedAt, UpdatedAt}) -> Session when
      SessionId :: xss_session:session_id(),
      CreatedAt :: xss_utils:epgsql_timestamp() | null,
      UpdatedAt :: xss_utils:epgsql_timestamp() | null,
      Session :: xss_session:session().
parse_db_row({SessionId, CreatedAt, UpdatedAt}) ->
      xss_session:new(
          #{session_id => SessionId,
            created_at =>
             xss_utils:epgsql_timestamp_to_xss_timestamp(CreatedAt),
            updated_at =>
             xss_utils:epgsql_timestamp_to_xss_timestamp(UpdatedAt)}).
