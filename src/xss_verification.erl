%%%-----------------------------------------------------------------------------
%%% @doc A {@link verification()} represents an attempt where the system was
%%%      trying to verify a user's motion sample.
%%% @end
%%%-----------------------------------------------------------------------------

-module(xss_verification).
-include("x11_sentinel_server.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

%% Constructor
-export([new/1]).

%% Getters
-export([get_verification_id/1,
         get_profile_id/1,
         get_stream_id/1,
         get_last_chunk/1,
         get_chunk_count/1,
         get_result/1,
         get_succeeded_at/1,
         get_failed_at/1,
         get_created_at/1,
         get_updated_at/1]).

-export_type([verification/0,
              verification_id/0]).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type verification_id() :: binary().

-type result() :: float().

-type verification() ::
        #{verification_id := verification_id(),
          profile_id := xss_profile:profile_id(),
          stream_id := xss_stream:stream_id(),
          last_chunk := xss_chunk:sequence_number(),
          chunk_count := pos_integer(),
          result := result() | undefined,
          succeeded_at := xss_utils:xss_timestamp() | undefined,
          failed_at := xss_utils:xss_timestamp() | undefined,
          created_at :=xss_utils:xss_timestamp() | undefined,
          updated_at :=xss_utils:xss_timestamp() | undefined}.

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%%-----------------------------------------------------------------------------
%%% Constructor
%%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Create a verification.
%% @end
%%------------------------------------------------------------------------------
-spec new(#{verification_id := VerificationId,
            profile_id := ProfileId,
            stream_id := StreamId,
            last_chunk := LastChunk,
            chunk_count := ChunkCount,
            result => Result,
            succeeded_at => SucceededAt,
            failed_at => FailedAt,
            created_at => CreatedAt,
            updated_at => UpdatedAt}) -> Verification when
      VerificationId :: verification_id(),
      ProfileId :: xss_profile:profile_id(),
      StreamId :: xss_stream:stream_id(),
      LastChunk :: xss_chunk:sequence_number(),
      ChunkCount :: pos_integer(),
      Result :: result() | undefined,
      SucceededAt :: xss_utils:xss_timestamp() | undefined,
      FailedAt :: xss_utils:xss_timestamp() | undefined,
      CreatedAt :: xss_utils:xss_timestamp() | undefined,
      UpdatedAt :: xss_utils:xss_timestamp() | undefined,
      Verification :: verification().
new(#{verification_id := VerificationId,
      profile_id := ProfileId,
      stream_id := StreamId,
      last_chunk := LastChunk,
      chunk_count := ChunkCount} = Args) ->
    #{verification_id => VerificationId,
      profile_id => ProfileId,
      stream_id => StreamId,
      last_chunk => LastChunk,
      chunk_count => ChunkCount,
      result => maps:get(result, Args, undefined),
      succeeded_at => maps:get(succeeded_at, Args, undefined),
      failed_at => maps:get(failed_at, Args, undefined),
      created_at => maps:get(created_at, Args, undefined),
      updated_at => maps:get(updated_at, Args, undefined)}.

%%%-----------------------------------------------------------------------------
%%% Getters
%%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Get the verification ID.
%% @end
%%------------------------------------------------------------------------------
-spec get_verification_id(Verification) -> VerificationId when
      Verification :: verification(),
      VerificationId :: verification_id().
get_verification_id(#{verification_id := VerificationId}) ->
    VerificationId.

%%------------------------------------------------------------------------------
%% @doc Get the profile ID.
%% @end
%%------------------------------------------------------------------------------
-spec get_profile_id(Verification) -> ProfileId when
      Verification :: verification(),
      ProfileId :: xss_profile:profile_id().
get_profile_id(#{profile_id := ProfileId}) ->
    ProfileId.

%%------------------------------------------------------------------------------
%% @doc Get the stream ID.
%% @end
%%------------------------------------------------------------------------------
-spec get_stream_id(Verification) -> StreamId when
      Verification :: verification(),
      StreamId :: xss_stream:stream_id().
get_stream_id(#{stream_id := StreamId}) ->
    StreamId.

%%------------------------------------------------------------------------------
%% @doc Get the sequence number of the last chunk.
%% @end
%%------------------------------------------------------------------------------
-spec get_last_chunk(Verification) -> LastChunk when
      Verification :: verification(),
      LastChunk :: xss_chunk:sequence_number().
get_last_chunk(#{last_chunk := LastChunk}) ->
    LastChunk.

%%------------------------------------------------------------------------------
%% @doc Get the number of chunks in the verified sample.
%% @end
%%------------------------------------------------------------------------------
-spec get_chunk_count(Verification) -> ChunkCount when
      Verification :: verification(),
      ChunkCount :: pos_integer().
get_chunk_count(#{chunk_count := ChunkCount}) ->
    ChunkCount.

%%------------------------------------------------------------------------------
%% @doc Get the verification result.
%% @end
%%------------------------------------------------------------------------------
-spec get_result(Verification) -> Result when
      Verification :: verification(),
      Result :: result().
get_result(#{result := Result}) ->
    Result.

%%------------------------------------------------------------------------------
%% @doc Get when the verification succeeded.
%% @end
%%------------------------------------------------------------------------------
-spec get_succeeded_at(Verification) -> SucceededAt when
      Verification :: verification(),
      SucceededAt :: xss_utils:xss_timestamp() | undefined.
get_succeeded_at(#{succeeded_at := SucceededAt}) ->
    SucceededAt.

%%------------------------------------------------------------------------------
%% @doc Get when the verification failed.
%% @end
%%------------------------------------------------------------------------------
-spec get_failed_at(Verification) -> FailedAt when
      Verification :: verification(),
      FailedAt :: xss_utils:xss_timestamp() | undefined.
get_failed_at(#{failed_at := FailedAt}) ->
    FailedAt.

%%------------------------------------------------------------------------------
%% @doc Get when the verification was started.
%% @end
%%------------------------------------------------------------------------------
-spec get_created_at(Verification) -> CreatedAt when
      Verification :: verification(),
      CreatedAt :: xss_utils:xss_timestamp().
get_created_at(#{created_at := CreatedAt}) ->
    CreatedAt.

%%------------------------------------------------------------------------------
%% @doc Get when the verification was updated.
%% @end
%%------------------------------------------------------------------------------
-spec get_updated_at(Verification) -> UpdatedAt when
      Verification :: verification(),
      UpdatedAt :: xss_utils:xss_timestamp().
get_updated_at(#{updated_at := UpdatedAt}) ->
    UpdatedAt.
