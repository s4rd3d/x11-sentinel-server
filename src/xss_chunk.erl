%%%-----------------------------------------------------------------------------
%%% @doc The chunk model represents the smallest part of a data stream of the
%%%      `x11-sentinel-client' application.
%%% @end
%%%-----------------------------------------------------------------------------

-module(xss_chunk).
-include("x11_sentinel_server.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

%% API
-export([new/4]).

%% Getters and custom data representations
-export([get_user_id/1,
         get_stream_id/1,
         get_session_id/1,
         get_sequence_number/1,
         get_submitted_at/1,
         get_events/1,
         get_metadata/1,
         get_epoch/1,
         get_created_at/1,
         get_updated_at/1]).

-export([calculate_event_count/1]).

-export_type([chunk/0,
              sequence_number/0,
              event/0]).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type sequence_number() :: non_neg_integer().

-type event() :: [any()].

-type chunk() ::
        #{metadata :=
              #{user_id := xss_user:user_id(),
                session_id := xss_session:session_id(),
                stream_id := xss_stream:stream_id(),
                sequence_number := sequence_number(),
                epoch := #{unit := binary(),
                           value := non_neg_integer()}},
          submitted_at := xss_utils:xss_timestamp(),
          real_ip_address := binary(),
          peer_ip_address := binary(),
          referer := binary(),
          chunk := [event()],
          created_at := xss_utils:xss_timestamp() | undefined,
          updated_at := xss_utils:xss_timestamp() | undefined}.

%%%=============================================================================
%%% Exported functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Check and extend input and compile a new, valid chunk object.
%% @end
%%------------------------------------------------------------------------------
-spec new(LooseChunk, RealIpAddress, PeerIpAddress, Referer) -> ValidChunk when
      LooseChunk :: chunk() | #{binary() => binary()},
      RealIpAddress :: inet:ip_address(),
      PeerIpAddress :: inet:ip_address(),
      Referer :: binary(),
      ValidChunk :: chunk().
new(#{} = Chunk, RealIpAddress, PeerIpAddress, Referer) when
      is_binary(Referer) ->
    maps:merge(
      Chunk,
      #{submitted_at => xss_utils:now(),
        real_ip_address => list_to_binary(inet:ntoa(RealIpAddress)),
        peer_ip_address => list_to_binary(inet:ntoa(PeerIpAddress)),
        referer => Referer,
        created_at => undefined,
        updated_at => undefined}).

%%------------------------------------------------------------------------------
%% @doc Return a user id for a chunk.
%% @end
%%------------------------------------------------------------------------------
-spec get_user_id(Chunk) -> UserId when
      Chunk :: chunk(),
      UserId :: binary().
get_user_id(#{metadata := #{user_id := UserId}}) ->
    UserId.

%%------------------------------------------------------------------------------
%% @doc Return a stream id for a chunk.
%% @end
%%------------------------------------------------------------------------------
-spec get_stream_id(Chunk) -> StreamId when
    Chunk :: chunk(),
    StreamId :: binary().
get_stream_id(#{metadata := #{stream_id := StreamId}}) ->
    StreamId.

%%------------------------------------------------------------------------------
%% @doc Return a session id for a chunk.
%% @end
%%------------------------------------------------------------------------------
-spec get_session_id(Chunk) -> SessionId when
    Chunk :: chunk(),
    SessionId :: binary().
get_session_id(#{metadata := #{session_id := SessionId}}) ->
    SessionId.

%%------------------------------------------------------------------------------
%% @doc Return sequence number for a chunk.
%% @end
%%------------------------------------------------------------------------------
-spec get_sequence_number(Chunk) -> SequenceNumber when
    Chunk :: chunk(),
    SequenceNumber :: sequence_number().
get_sequence_number(#{metadata := #{sequence_number := SequenceNumber}}) ->
    SequenceNumber.

%%------------------------------------------------------------------------------
%% @doc Return "submitted at" for a chunk.
%% @end
%%------------------------------------------------------------------------------
-spec get_submitted_at(Chunk) -> SubmittedAt when
      Chunk :: chunk(),
      SubmittedAt :: binary().
get_submitted_at(#{submitted_at := SubmittedAt}) ->
    SubmittedAt.

%%------------------------------------------------------------------------------
%% @doc Return the events of a chunk.
%% @end
%%------------------------------------------------------------------------------
-spec get_events(Chunk) -> Events when
    Chunk :: chunk(),
    Events :: [event()].
get_events(#{chunk := Events}) ->
    Events.

%%------------------------------------------------------------------------------
%% @doc Return the chunk's metadata.
%% @end
%%------------------------------------------------------------------------------
-spec get_metadata(Chunk) -> Metadata when
      Chunk :: chunk(),
      Metadata :: map().
get_metadata(#{metadata := Metadata}) ->
    Metadata.

%%------------------------------------------------------------------------------
%% @doc Return the epoch of a chunk's metadata.
%% @end
%%------------------------------------------------------------------------------
-spec get_epoch(Chunk) -> Epoch when
      Chunk :: chunk(),
      Epoch :: non_neg_integer().
get_epoch(#{metadata := #{epoch := #{value := Epoch}}}) ->
    Epoch.

%%-----------------------------------------------------------------------------
%% @doc Get the `created_at' field of the chunk.
%% @end
%%-----------------------------------------------------------------------------
-spec get_created_at(Chunk) -> CreatedAt when
      Chunk :: chunk(),
      CreatedAt :: xss_utils:xss_timestamp() | undefined.
get_created_at(#{created_at := CreatedAt}) ->
    CreatedAt.

%%-----------------------------------------------------------------------------
%% @doc Get the `updated_at' field of the chunk.
%% @end
%%-----------------------------------------------------------------------------
-spec get_updated_at(Chunk) -> UpdatedAt when
      Chunk :: chunk(),
      UpdatedAt :: xss_utils:xss_timestamp() | undefined.
get_updated_at(#{updated_at := UpdatedAt}) ->
    UpdatedAt.

%%------------------------------------------------------------------------------
%% @doc Return the event count in a chunk.
%% @end
%%------------------------------------------------------------------------------
-spec calculate_event_count(Chunk) -> EventCount when
    Chunk :: chunk(),
    EventCount :: non_neg_integer().
calculate_event_count(Chunk) ->
    length(get_events(Chunk)).
