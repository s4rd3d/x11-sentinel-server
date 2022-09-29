%%%-----------------------------------------------------------------------------
%%% @doc This module contains API functions to execute queries on the database
%%%      related to the {@link xss_verification} model.
%%% @end
%%%-----------------------------------------------------------------------------

-module(xss_verification_store).
-include("x11_sentinel_server.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

-export([select_verification_by_verification_id/1,
         select_verifications_by_profile_id/1,
         select_latest_succeeded_verification_by_user_id/1,
         insert_verification/1,
         update_verification_success/2,
         update_verification_failure/1,
         soft_delete_verification_by_verification_id/1]).

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Get a verification from the database by the `verification_id' field.
%% @end
%%------------------------------------------------------------------------------
-spec select_verification_by_verification_id(VerificationId) -> Result when
      VerificationId :: xss_verification:verification_id(),
      Result :: {ok, xss_verification:verification()} | {error, Reason},
      Reason :: any().
select_verification_by_verification_id(VerificationId) ->
    case
      xss_database_server:execute(select_verification_by_verification_id,
                                  [VerificationId])
    of
        {ok, _Columns, []} ->
            {error, #{reason => <<"Verification does not exist.">>,
                      verification_id => VerificationId}};
        {ok, _Columns, [Row | _Rest]} ->
            {ok, parse_db_row(Row)}
    end.

%%------------------------------------------------------------------------------
%% @doc Get verifications from the database by the `profile_id' field.
%% @end
%%------------------------------------------------------------------------------
-spec select_verifications_by_profile_id(ProfileId) -> Result when
      ProfileId :: xss_profile:profile_id(),
      Result :: {ok, [xss_verification:verification()]}.
select_verifications_by_profile_id(ProfileId) ->
    case
      xss_database_server:execute(select_verifications_by_profile_id,
                                  [ProfileId])
    of
        {ok, _Columns, []} ->
            {ok, []};
        {ok, _Columns, Rows} ->
            {ok, lists:map(fun parse_db_row/1, Rows)}
    end.

%%------------------------------------------------------------------------------
%% @doc Get the latest successful verification from the database by the
%%      `user_id' field.
%% @end
%%------------------------------------------------------------------------------
-spec select_latest_succeeded_verification_by_user_id(UserId) -> Result when
      UserId :: xss_user:user_id(),
      Result :: {ok, xss_verification:verification()} | {error, Reason},
      Reason :: any().
select_latest_succeeded_verification_by_user_id(UserId) ->
    case
      xss_database_server:execute(select_latest_succeeded_verification_by_user_id,
                                  [UserId])
    of
        {ok, _Columns, []} ->
            {error, #{reason => <<"Verification not found.">>,
                      user_id => UserId}};
        {ok, _Columns, [Row | _Rest]} ->
            {ok, parse_db_row(Row)}
    end.

%%------------------------------------------------------------------------------
%% @doc Insert a new verification into the database.
%% @end
%%------------------------------------------------------------------------------
-spec insert_verification(Verification) -> Result when
      Verification :: xss_verification:verification(),
      Result :: {ok, RowsEffected} | {error, Reason},
      RowsEffected :: integer(),
      Reason :: any().
insert_verification(#{verification_id := VerificationId,
                      profile_id := ProfileId,
                      stream_id := StreamId,
                      last_chunk := LastChunk,
                      chunk_count := ChunkCount,
                      result := undefined,
                      succeeded_at := undefined,
                      failed_at := undefined,
                      created_at := undefined,
                      updated_at := undefined
                    }) ->
    Now = xss_utils:xss_timestamp_to_epgsql_timestamp(xss_utils:now()),
    xss_database_server:execute(insert_verification,
                                [VerificationId,
                                 ProfileId,
                                 StreamId,
                                 LastChunk,
                                 ChunkCount,
                                 Now,
                                 Now]).

%%------------------------------------------------------------------------------
%% @doc Mark verification as "succeeded" in the database. Add result of the
%%      verification to the database.
%% @end
%%------------------------------------------------------------------------------
-spec update_verification_success(VerificationId,
                                  VerificationResult) -> Result when
      VerificationId :: xss_verification:verification_id(),
      VerificationResult :: float(),
      Result :: {ok, RowsEffected} | {error, Reason},
      RowsEffected :: integer(),
      Reason :: any().
update_verification_success(VerificationId, VerificationResult) ->
    Now = xss_utils:xss_timestamp_to_epgsql_timestamp(xss_utils:now()),
    xss_database_server:execute(update_verification_success,
                                [VerificationResult, Now, Now, VerificationId]).

%%------------------------------------------------------------------------------
%% @doc Mark verification as "failed" in the database.
%% @end
%%------------------------------------------------------------------------------
-spec update_verification_failure(VerificationId) -> Result when
      VerificationId :: xss_verification:verification_id(),
      Result :: {ok, RowsEffected} | {error, Reason},
      RowsEffected :: integer(),
      Reason :: any().
update_verification_failure(VerificationId) ->
    Now = xss_utils:xss_timestamp_to_epgsql_timestamp(xss_utils:now()),
    xss_database_server:execute(update_verification_failure,
                                [Now, Now, VerificationId]).

%%------------------------------------------------------------------------------
%% @doc Soft delete verification by setting the `deleted_at' field.
%% @end
%%------------------------------------------------------------------------------
-spec soft_delete_verification_by_verification_id(VerificationId) -> Result when
      VerificationId :: xss_verification:verification_id(),
      Result :: {ok, RowsEffected} | {error, Reason},
      RowsEffected :: integer(),
      Reason :: any().
soft_delete_verification_by_verification_id(VerificationId) ->
    Now = xss_utils:xss_timestamp_to_epgsql_timestamp(xss_utils:now()),
    xss_database_server:execute(soft_delete_verification_by_verification_id,
                                [Now, Now, VerificationId]).

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Parse a raw database entry into a {@link xss_verification} model entity.
%% @end
%%-----------------------------------------------------------------------------
-spec parse_db_row({VerificationId,
                    ProfileId,
                    StreamId,
                    LastChunk,
                    ChunkCount,
                    Result,
                    SucceededAt,
                    FailedAt,
                    CreatedAt,
                    UpdatedAt}) -> Verification when
      VerificationId :: xss_verification:verification_id(),
      ProfileId :: xss_profile:profile_id(),
      StreamId :: xss_stream:stream_id(),
      LastChunk :: xss_chunk:sequence_number(),
      ChunkCount :: pos_integer(),
      Result :: float() | null,
      SucceededAt :: xss_utils:epgsql_timestamp() | null,
      FailedAt :: xss_utils:epgsql_timestamp() | null,
      CreatedAt :: xss_utils:epgsql_timestamp() | null,
      UpdatedAt :: xss_utils:epgsql_timestamp() | null,
      Verification :: xss_verification:verification().
parse_db_row({VerificationId,
              ProfileId,
              StreamId,
              LastChunk,
              ChunkCount,
              Result,
              SucceededAt,
              FailedAt,
              CreatedAt,
              UpdatedAt}) ->
    xss_verification:new(
        #{verification_id => VerificationId,
          profile_id => ProfileId,
          stream_id => StreamId,
          last_chunk => LastChunk,
          chunk_count => ChunkCount,
          result => xss_utils:null_to_undefined(Result),
          succeeded_at =>
           xss_utils:epgsql_timestamp_to_xss_timestamp(SucceededAt),
          failed_at =>
           xss_utils:epgsql_timestamp_to_xss_timestamp(FailedAt),
          created_at =>
           xss_utils:epgsql_timestamp_to_xss_timestamp(CreatedAt),
          updated_at =>
           xss_utils:epgsql_timestamp_to_xss_timestamp(UpdatedAt)}).
