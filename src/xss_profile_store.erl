%%%-----------------------------------------------------------------------------
%%% @doc This module contains API functions to execute queries on the database
%%%      related to the {@link xss_profile} model.
%%% @end
%%%-----------------------------------------------------------------------------

-module(xss_profile_store).
-include("x11_sentinel_server.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

-export([select_profile_by_profile_id/1,
         select_latest_profile_by_user_id/1,
         insert_profile/1,
         update_profile_success/2,
         update_profile_failure/1,
         soft_delete_profile_by_profile_id/1]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get a profile from the database by the `profile_id' field.
%% @end
%%------------------------------------------------------------------------------
-spec select_profile_by_profile_id(ProfileId) -> Result when
      ProfileId :: xss_profile:profile_id(),
      Result :: {ok, xss_profile:profile()} | {error, Reason},
      Reason :: any().
select_profile_by_profile_id(ProfileId) ->
    case
      xss_database_server:execute(select_profile_by_profile_id, [ProfileId])
    of
        {ok, _Columns, []} ->
            {error, #{reason => profile_not_found,
                      profile_id => ProfileId}};
        {ok, _Columns, [Row | _Rest]} ->
            {ok, parse_db_row(Row)}
    end.

%%------------------------------------------------------------------------------
%% @doc Get the latest profile from the database by the `user_id' field.
%% @end
%%------------------------------------------------------------------------------
-spec select_latest_profile_by_user_id(UserId) -> Result when
      UserId :: xss_user:user_id(),
      Result :: {ok, xss_profile:profile()} | {error, Reason},
      Reason :: any().
select_latest_profile_by_user_id(UserId) ->
    case
      xss_database_server:execute(select_latest_profile_by_user_id, [UserId])
    of
        {ok, _Columns, []} ->
            {error, #{reason => profile_not_found,
                      user_id => UserId}};
        {ok, _Columns, [Row | _Rest]} ->
            {ok, parse_db_row(Row)}
    end.

%%------------------------------------------------------------------------------
%% @doc Insert a new profile into the database.
%% @end
%%------------------------------------------------------------------------------
-spec insert_profile(Profile) -> Result when
      Profile :: xss_profile:profile(),
      Result :: {ok, RowsEffected} | {error, Reason},
      RowsEffected :: integer(),
      Reason :: any().
insert_profile(#{profile_id := ProfileId,
                 user_id := UserId,
                 profile_data := undefined,
                 succeeded_at := undefined,
                 failed_at := undefined,
                 created_at := undefined,
                 updated_at := undefined
                }) ->
    Now = xss_utils:xss_timestamp_to_epgsql_timestamp(xss_utils:now()),
    xss_database_server:execute(insert_profile,
                                [ProfileId, UserId, Now, Now]).

%%------------------------------------------------------------------------------
%% @doc Mark profile building as "succeeded" in the database. Add binary
%%      profile data to the profile entry.
%% @end
%%------------------------------------------------------------------------------
-spec update_profile_success(ProfileId, ProfileData) -> Result when
      ProfileId :: xss_profile:profile_id(),
      ProfileData :: xss_profile:profile_data(),
      Result :: {ok, RowsEffected} | {error, Reason},
      RowsEffected :: integer(),
      Reason :: any().
update_profile_success(ProfileId, ProfileData) ->
    Now = xss_utils:xss_timestamp_to_epgsql_timestamp(xss_utils:now()),
    xss_database_server:execute(update_profile_success,
                                [ProfileData, Now, Now, ProfileId]).

%%------------------------------------------------------------------------------
%% @doc Mark profile building as "failed" in the database.
%% @end
%%------------------------------------------------------------------------------
-spec update_profile_failure(ProfileId) -> Result when
      ProfileId :: xss_profile:profile_id(),
      Result :: {ok, RowsEffected} | {error, Reason},
      RowsEffected :: integer(),
      Reason :: any().
update_profile_failure(ProfileId) ->
    Now = xss_utils:xss_timestamp_to_epgsql_timestamp(xss_utils:now()),
    xss_database_server:execute(update_profile_failure,
                                [Now, Now, ProfileId]).

%%------------------------------------------------------------------------------
%% @doc Soft delete profile by setting the `deleted_at' field.
%% @end
%%------------------------------------------------------------------------------
-spec soft_delete_profile_by_profile_id(ProfileId) -> Result when
      ProfileId :: xss_profile:profile_id(),
      Result :: {ok, RowsEffected} | {error, Reason},
      RowsEffected :: integer(),
      Reason :: any().
soft_delete_profile_by_profile_id(ProfileId) ->
    Now = xss_utils:xss_timestamp_to_epgsql_timestamp(xss_utils:now()),
    xss_database_server:execute(soft_delete_profile_by_profile_id,
                                [Now, Now, ProfileId]).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Parse a raw database entry into a {@link xss_profile} model entity.
%% @end
%%-----------------------------------------------------------------------------
-spec parse_db_row({ProfileId,
                    UserId,
                    ProfileData,
                    SucceededAt,
                    FailedAt,
                    CreatedAt,
                    UpdatedAt}) -> Profile when
      ProfileId :: xss_profile:profile_id(),
      UserId :: xss_user:user_id(),
      ProfileData :: xss_profile:profile_data(),
      SucceededAt :: xss_utils:epgsql_timestamp() | null,
      FailedAt :: xss_utils:epgsql_timestamp() | null,
      CreatedAt :: xss_utils:epgsql_timestamp() | null,
      UpdatedAt :: xss_utils:epgsql_timestamp() | null,
      Profile :: xss_profile:profile().
parse_db_row({ProfileId,
              UserId,
              ProfileData,
              SucceededAt,
              FailedAt,
              CreatedAt,
              UpdatedAt}) ->
    xss_profile:new(
        #{profile_id => xss_utils:null_to_undefined(ProfileId),
          user_id => UserId,
          profile_data => xss_utils:null_to_undefined(ProfileData),
          succeeded_at =>
           xss_utils:epgsql_timestamp_to_xss_timestamp(SucceededAt),
          failed_at =>
           xss_utils:epgsql_timestamp_to_xss_timestamp(FailedAt),
          created_at =>
           xss_utils:epgsql_timestamp_to_xss_timestamp(CreatedAt),
          updated_at =>
           xss_utils:epgsql_timestamp_to_xss_timestamp(UpdatedAt)}).
