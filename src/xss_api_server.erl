%%%-----------------------------------------------------------------------------
%%% @doc Generic server module that provides an API for tasks which need to be
%%%      done asynchronously.
%%% @end
%%%-----------------------------------------------------------------------------

-module(xss_api_server).
-include("x11_sentinel_server.hrl").
-behaviour(gen_server).

%%%=============================================================================
%%% Exports
%%%=============================================================================

%% `gen_server' API
-export([start_link/0]).

%% `gen_server' callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         terminate/2
        ]).

%% API functions
-export([after_persist/2]).

%% Test only exports
-ifdef(TEST).
-export([decide_action/1]).
-endif. % -ifdef(TEST)


%%%=============================================================================
%%% Macros
%%%=============================================================================

-define(TIMEOUT, 5000). % 5 seconds
-define(PROFILE_BUILD_TIMEOUT, 300000). % 5 minutes
-define(VERIFY_TIMEOUT, 60000). % 1 minute

%%%=============================================================================
%%% Records
%%%=============================================================================

%% Internal `gen_server' state
-record(state, {}).

%%%=============================================================================
%%% Types
%%%=============================================================================

%% Internal `gen_server' state
-type state() :: #state{}.

%%%=============================================================================
%%% Exported functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Perform various tasks and execute callbacks after a chunk is persisted
%%      into the database.
%% @end
%%------------------------------------------------------------------------------
-spec after_persist(UserId, Chunk) -> ok when
      UserId :: xss_user:user_id(),
      Chunk :: xss_chunk:chunk().
after_persist(UserId, Chunk) ->
    gen_server:cast(?MODULE, {after_persist, UserId, Chunk}).

%%------------------------------------------------------------------------------
%% @doc Start `gen_server'.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          [],
                          [{timeout, ?TIMEOUT}]).

%%------------------------------------------------------------------------------
%% @doc Initialize the server.
%% @end
%%------------------------------------------------------------------------------
-spec init(Args) -> {ok, State} when
      Args :: [any()],
      State :: state().
init([]) ->
    {ok, _InitialState = #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handle synchronous requests.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(Request, From, State) -> Result when
      Request :: term(),
      From :: {pid(), Tag},
      Tag :: term(),
      State :: state(),
      Result :: {reply, Reply, state()} |
                {reply, Reply, state(), timeout()} |
                {noreply, state()} |
                {noreply, state(), timeout()} |
                {stop, Reason, Reply, state()} |
                {stop, Reason, state()},
      Reply :: term(),
      Reason :: term().
handle_call(Request, From, State) ->
    ok = logger:warning(#{message => "Unhandled handle_call/3",
                          request => Request,
                          from => From,
                          state => State}),
    {reply, {bad_request, ?MODULE, Request}, State}.

%%------------------------------------------------------------------------------
%% @doc Handle asynchronous requests.
%% @end
%%------------------------------------------------------------------------------
-spec handle_cast(Msg, State) -> Result when
      Msg :: term(),
      State :: state(),
      Result :: {noreply, state()} |
                {noreply, state(), timeout()} |
                {stop, Reason, state()},
      Reason :: term().
handle_cast({after_persist, UserId, Chunk}, State) ->
    % 1. Save user - session and user - stream to the database if needed.
    SessionId = xss_chunk:get_session_id(Chunk),
    StreamId = xss_chunk:get_stream_id(Chunk),
    ok = maybe_add_user_session(UserId, SessionId),
    ok = maybe_add_user_stream(UserId, StreamId),

    % 2. Build a profile or execute a verification if needed.
    ok = maybe_build_profile_or_verify(UserId),

    {noreply, State};

handle_cast({build_profile, #{user_id := UserId} = Profile}, State) ->
    {ok, Chunks} = xss_chunk_store:select_chunks_by_user_id(UserId),

    case
        call_build_profile(Chunks)
    of
        {ok, ProfileData} ->
            ProfileId = xss_profile:get_profile_id(Profile),
            {ok, 1} = xss_profile_store:update_profile_success(ProfileId,
                                                               ProfileData),
            ok = logger:info(#{message => <<"Profile build succesful.">>,
                               profile_id => ProfileId});
        {error, Reason} ->
            ProfileId = xss_profile:get_profile_id(Profile),
            {ok, 1} = xss_profile_store:update_profile_failure(ProfileId),
            ok = logger:warning(#{message => <<"Profile build failed.">>,
                                  reason => Reason,
                                  profile_id => ProfileId})
    end,

    {noreply, State};

handle_cast({verify, #{user_id := UserId} = Profile}, State) ->
    {ok, Stream} = xss_stream_store:select_latest_stream_by_user_id(UserId),
    StreamId = xss_stream:get_stream_id(Stream),
    {ok, SequenceNumbers} =
     xss_chunk_store:select_sequence_numbers_by_stream_id(StreamId),

    case
        get_chunks_for_verify(StreamId, SequenceNumbers)
    of
        {ok, Chunks} ->
            case
                call_verify(Chunks, Profile, StreamId)
            of
                {success, Verification, Result} ->
                    VerificationId = xss_verification:get_verification_id(Verification),
                    {ok, 1} = xss_verification_store:update_verification_success(
                        VerificationId, Result),
                    ok = logger:info(#{message => <<"Verification successful.">>,
                                       user_id => UserId,
                                       verification_id => VerificationId,
                                       result => Result});

                {failure, Verification, Reason} ->
                    VerificationId = xss_verification:get_verification_id(Verification),
                    {ok, 1} = xss_verification_store:update_verification_failure(VerificationId),
                    ok = logger:warning(#{message => <<"Verification failed.">>,
                                          reason => Reason,
                                          user_id => UserId,
                                          verification_id => VerificationId})
            end;

        {error, Reason} ->
            ok = logger:warning(#{message => <<"Verification failed.">>,
                                  reason => Reason})
    end,

    {noreply, State};

handle_cast(Request, State) ->
    ok = logger:warning(#{message => "Unhandled handle_cast/2",
                          request => Request,
                          state => State}),
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @doc Clean up the gen_server state.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(Reason, State) -> any() when
      Reason :: term(),
      State :: state().
terminate(_Reason, _State) ->
    ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Maybe add a user - session pair to the database.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_user_session(UserId, SessionId) -> ok when
      UserId :: xss_user:user_id(),
      SessionId :: xss_session:session_id().
maybe_add_user_session(UserId, SessionId) ->
    case
        xss_database_server:execute(
            select_user_session_by_user_id_and_session_id, [UserId, SessionId])
    of
        {ok, _Columns, [_Row | _Rest]} ->
            ok;
        {ok, _Columns, []} ->
            ok = logger:info(#{message => <<"Adding new user-session">>,
                               session_id => SessionId,
                               user_id => UserId}),
            {ok, _} = xss_database_server:execute(insert_user_session,
                                                  [UserId, SessionId]),
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc Maybe add a user - stream pair to the database.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_add_user_stream(UserId, StreamId) -> ok when
      UserId :: xss_user:user_id(),
      StreamId :: xss_stream:stream_id().
maybe_add_user_stream(UserId, StreamId) ->
    case
        xss_database_server:execute(
            select_user_stream_by_user_id_and_stream_id, [UserId, StreamId])
    of
        {ok, _Columns, [_Row | _Rest]} ->
            ok;
        {ok, _Columns, []} ->
            ok = logger:info(#{message => <<"Adding new user-stream">>,
                               stream_id => StreamId,
                               user_id => UserId}),
            {ok, _} = xss_database_server:execute(insert_user_stream,
                                                 [UserId, StreamId]),
            ok
    end.

%%------------------------------------------------------------------------------
%% @doc Maybe build a profile or execute a verification.
%% @end
%%------------------------------------------------------------------------------
-spec maybe_build_profile_or_verify(UserId) -> ok when
      UserId :: xss_user:user_id().
maybe_build_profile_or_verify(UserId) ->
    case
        xss_user_store:select_user_by_user_id(UserId)
    of
        {ok, User} ->
            case
                decide_action(User)
            of
                {build_profile, Profile} ->
                    gen_server:cast(?MODULE, {build_profile, Profile});
                {verify, Profile} ->
                    gen_server:cast(?MODULE, {verify, Profile});
                {nop, Reason} ->
                    ok = logger:info(#{message => <<"No action">>,
                                       reason => Reason})
            end;

        {error, #{reason := user_not_found}} ->
            ok = logger:warning(#{message => <<"Unknown user">>,
                                  user_id => UserId})
    end.

%%------------------------------------------------------------------------------
%% @doc Decide which action to be taken based on the user's status.
%% @end
%%------------------------------------------------------------------------------
-spec decide_action(User) -> Action when
      User :: xss_user:user(),
      Action :: {build_profile, Profile} |
                {verify, Profile} |
                {nop, Reason},
      Profile :: xss_profile:profile(),
      Reason :: profile_building_is_in_progress |
                cannot_rebuild_new_profile |
                not_enough_data_for_profile_building.
decide_action(User) ->
    UserId = xss_user:get_user_id(User),

    case
        xss_profile_store:select_latest_profile_by_user_id(UserId)
    of
        {ok, Profile} ->
            case
                xss_profile:is_in_progress(Profile)
            of
                true ->
                    {nop, profile_building_is_in_progress};
                false ->
                    case
                        xss_profile:is_succeeded(Profile)
                    of
                        true ->
                            {verify, Profile};
                        false ->
                            case
                                can_rebuild_profile(Profile)
                            of
                                true ->
                                    EmptyProfile = insert_empty_profile(UserId),
                                    {build_profile, EmptyProfile};
                                false ->
                                    {nop, cannot_rebuild_new_profile}
                            end
                    end

            end;

        {error, #{reason := profile_not_found}} ->
            case
                can_build_profile(User)
            of
                true ->
                    Profile = insert_empty_profile(UserId),
                    {build_profile, Profile};
                false ->
                    {nop, not_enough_data_for_profile_building}
            end
    end.

%%------------------------------------------------------------------------------
%% @doc Determine if there is enough data for a user to build a profile.
%% @end
%%------------------------------------------------------------------------------
-spec can_build_profile(User) -> boolean() when
      User :: xss_user:user().
can_build_profile(User) ->
    EventCount = xss_user:get_event_count(User),
    MinimumEventCount = xss_utils:minimum_event_count_for_profile(),
    EventCount >= MinimumEventCount.

%%------------------------------------------------------------------------------
%% @doc Check if enough time has passed to rebuild a failed profile.
%% @end
%%------------------------------------------------------------------------------
-spec can_rebuild_profile(Profile) -> boolean() when
      Profile :: xss_profile:profile().
can_rebuild_profile(#{failed_at := FailedAt} = Profile) when FailedAt =/= undefined ->
    Now = xss_utils:now(),
    UpdatedAt = xss_profile:get_updated_at(Profile),
    ProfileAge = Now - UpdatedAt,
    MinimumAge = xss_utils:minimum_elapsed_time_for_failed_profile_rebuild(),
    ProfileAge > MinimumAge.

%%------------------------------------------------------------------------------
%% @doc Insert a new empty profile to the database.
%% @end
%%------------------------------------------------------------------------------
-spec insert_empty_profile(UserId) -> Profile when
      UserId :: xss_user:user_id(),
      Profile :: xss_profile:profile().
insert_empty_profile(UserId) ->
    ProfileId = xss_utils:generate_uuid(),
    Profile = xss_profile:new(#{user_id => UserId, profile_id => ProfileId}),
    {ok, 1} = xss_profile_store:insert_profile(Profile),
    Profile.

%%------------------------------------------------------------------------------
%% @doc Prepare a list of chunks that is sufficient for a verification.
%% @end
%%------------------------------------------------------------------------------
-spec get_chunks_for_verify(StreamId, SequenceNumbers) -> Result when
      StreamId :: xss_stream:stream_id(),
      SequenceNumbers :: [xss_chunk:sequence_number()],
      Result :: {ok, Chunks} | {error, Reason},
      Chunks :: [xss_chunk:chunk()],
      Reason :: any().
get_chunks_for_verify(StreamId, SequenceNumbers) ->
    MinimumEventCount = xss_utils:minimum_event_count_for_verification(),
    do_get_chunks_for_verify([], 0, {StreamId, SequenceNumbers}, MinimumEventCount).

%%------------------------------------------------------------------------------
%% @doc Prepare a list of chunks that is sufficient for a verification.
%% @end
%%------------------------------------------------------------------------------
do_get_chunks_for_verify(Chunks,
                         EventCount,
                         {StreamId, [SequenceNumber | Rest]},
                         MinimumEventCount) when
      EventCount < MinimumEventCount ->
    {ok, NewChunk} = xss_chunk_store:select_chunk_by_stream_id_and_sequence_number(StreamId, SequenceNumber),
    NewEventCount = xss_chunk:calculate_event_count(NewChunk),
    do_get_chunks_for_verify([NewChunk | Chunks],
                             EventCount + NewEventCount,
                             {StreamId, Rest},
                             MinimumEventCount);
do_get_chunks_for_verify(_Chunks,
                         EventCount,
                         {_StreamId, []},
                         MinimumEventCount) when
      EventCount < MinimumEventCount ->
    {error, not_enough_data_for_verify};
do_get_chunks_for_verify(Chunks,
                         _EventCount,
                         {_StreamId, _SequenceNumbers},
                         _MinimumEventCount) ->
    {ok, Chunks}.

%%------------------------------------------------------------------------------
%% @doc Call the evaluation service to build a profile.
%% @end
%%------------------------------------------------------------------------------
-spec call_build_profile(Chunks) -> Result when
      Chunks :: [xss_chunk:chunk()],
      Result :: {ok, ProfileData} | {error, Reason},
      ProfileData :: iodata(),
      Reason :: any().
call_build_profile(Chunks) ->
    {ok, ConnPid} = gun:open(xss_utils:evaluation_service_host(),
                             xss_utils:evaluation_service_port()),
    StreamRef = gun:post(ConnPid,
                         "/profile",
                         [{<<"content-type">>, <<"application/json">>}],
                         jiffy:encode(#{chunks => Chunks})),
    {response, nofin, 200, _Headers} = gun:await(ConnPid, StreamRef, ?PROFILE_BUILD_TIMEOUT),
    {ok, Body} = gun:await_body(ConnPid, StreamRef, ?PROFILE_BUILD_TIMEOUT),
    ok = gun:shutdown(ConnPid),
    case
        xss_utils:camel_to_snake(jiffy:decode(Body, [return_maps]))
    of
        #{profile_data := ProfileData} ->
            {ok, ProfileData};
        Reason ->
            {error, Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc Call the evaluation service to verify a list of chunks against profile.
%% @end
%%------------------------------------------------------------------------------
-spec call_verify(Chunks, Profile, StreamId) -> Response when
      Chunks :: [xss_chunk:chunk()],
      Profile :: xss_profile:profile(),
      StreamId :: xss_stream:stream_id(),
      Response :: {success, Verification, Result} |
                  {failure, Verification, Reason},
      Result :: float(),
      Reason :: any().
call_verify(Chunks, Profile, StreamId) ->
    ProfileId = xss_profile:get_profile_id(Profile),
    VerificationId = xss_utils:generate_uuid(),
    LastChunk = xss_chunk:get_sequence_number(lists:last(Chunks)),
    ChunkCount = length(Chunks),

    Verification = xss_verification:new(#{verification_id => VerificationId,
                                          profile_id => ProfileId,
                                          stream_id => StreamId,
                                          last_chunk => LastChunk,
                                          chunk_count => ChunkCount}),
    {ok, 1} = xss_verification_store:insert_verification(Verification),

    case
        do_call_verify(Chunks, Profile)
    of
        {ok, Result} ->
            {success, Verification, Result};
        {error, Reason} ->
            {failure, Verification, Reason}
    end.

%%------------------------------------------------------------------------------
%% @doc Call the evaluation service to verify a list of chunks against profile.
%% @end
%%------------------------------------------------------------------------------
-spec do_call_verify(Chunks, Profile) -> Response when
      Chunks :: [xss_chunk:chunk()],
      Profile :: xss_profile:profile(),
      Response :: {ok, Result} | {error, Reason},
      Result :: float(),
      Reason :: any().
do_call_verify(Chunks, Profile) ->
    {ok, ConnPid} = gun:open(xss_utils:evaluation_service_host(),
                             xss_utils:evaluation_service_port()),
    StreamRef = gun:post(ConnPid,
                         "/verify",
                         [{<<"content-type">>, <<"application/json">>}],
                         jiffy:encode(#{chunks => Chunks, profile => Profile})),
    {response, nofin, 200, _Headers} = gun:await(ConnPid, StreamRef, ?VERIFY_TIMEOUT),
    {ok, Body} = gun:await_body(ConnPid, StreamRef, ?VERIFY_TIMEOUT),
    ok = gun:shutdown(ConnPid),
    case
        xss_utils:camel_to_snake(jiffy:decode(Body, [return_maps]))
    of
        #{result := Result} ->
            {ok, Result};
        Reason ->
            {error, Reason}
    end.

