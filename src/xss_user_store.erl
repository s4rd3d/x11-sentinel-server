%%%-----------------------------------------------------------------------------
%%% @doc This module contains API functions to execute queries on the database
%%%      related to the {@link xss_user} model.
%%% @end
%%%-----------------------------------------------------------------------------

-module(xss_user_store).
-include("x11_sentinel_server.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

-export([select_user_by_user_id/1,
         insert_user/1,
         update_user_event_count/2,
         soft_delete_user_by_user_id/1]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get a user from the database by the `user_id' field.
%% @end
%%------------------------------------------------------------------------------
-spec select_user_by_user_id(UserId) -> Result when
      UserId :: xss_user:user_id(),
      Result :: {ok, xss_user:user()} | {error, Reason},
      Reason :: any().
select_user_by_user_id(UserId) ->
    case
      xss_database_server:execute(select_user_by_user_id, [UserId])
    of
        {ok, _Columns, []} ->
            {error, #{reason => <<"User does not exist.">>,
                      user_id => UserId}};
        {ok, _Columns, [Row | _Rest]} ->
            {ok, parse_db_row(Row)}
    end.

%%------------------------------------------------------------------------------
%% @doc Insert a new user into the database.
%% @end
%%------------------------------------------------------------------------------
-spec insert_user(User) -> Result when
      User :: xss_user:user(),
      Result :: {ok, RowsEffected} | {error, Reason},
      RowsEffected :: integer(),
      Reason :: any().
insert_user(#{user_id := UserId,
              event_count := EventCount,
              created_at := undefined,
              updated_at := undefined}) ->
    Now = xss_utils:xss_timestamp_to_epgsql_timestamp(xss_utils:now()),
    xss_database_server:execute(insert_user, [UserId, EventCount, Now, Now]).

%%------------------------------------------------------------------------------
%% @doc Update the `event_count' field of a user.
%% @end
%%------------------------------------------------------------------------------
-spec update_user_event_count(User, EventCount) -> Result when
      User :: xss_user:user(),
      EventCount :: non_neg_integer() | undefined,
      Result :: {ok, RowsEffected} | {error, Reason},
      RowsEffected :: integer(),
      Reason :: any().
update_user_event_count(#{user_id := UserId}, EventCount) ->
    Now = xss_utils:xss_timestamp_to_epgsql_timestamp(xss_utils:now()),
    xss_database_server:execute(update_user_event_count,
                                [EventCount, Now, UserId]).

%%------------------------------------------------------------------------------
%% @doc Soft delete user by setting the `deleted_at' field.
%% @end
%%------------------------------------------------------------------------------
-spec soft_delete_user_by_user_id(UserId) -> Result when
      UserId :: xss_user:user_id(),
      Result :: {ok, RowsEffected} | {error, Reason},
      RowsEffected :: integer(),
      Reason :: any().
soft_delete_user_by_user_id(UserId) ->
    Now = xss_utils:xss_timestamp_to_epgsql_timestamp(xss_utils:now()),
    xss_database_server:execute(soft_delete_user_by_user_id,
                                [Now, Now, UserId]).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Parse a raw database entry into a {@link xss_user} model entity.
%% @end
%%-----------------------------------------------------------------------------
-spec parse_db_row({UserId, EventCount, CreatedAt, UpdatedAt}) -> User when
      UserId :: xss_user:user_id(),
      EventCount :: non_neg_integer() | undefined,
      CreatedAt :: integer() | undefined,
      UpdatedAt :: integer() | undefined,
      User :: xss_user:user().
parse_db_row({UserId, EventCount, CreatedAt, UpdatedAt}) ->
      xss_user:new(#{user_id => UserId,
                     event_count => EventCount,
                     created_at =>
                      xss_utils:epgsql_timestamp_to_xss_timestamp(CreatedAt),
                     updated_at =>
                      xss_utils:epgsql_timestamp_to_xss_timestamp(UpdatedAt)}).
