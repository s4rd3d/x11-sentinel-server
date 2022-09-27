%%%-----------------------------------------------------------------------------
%%% @doc A {@link profile()} represents a biometric profile (personal model of
%%%      a user).
%%% @end
%%%-----------------------------------------------------------------------------

-module(xss_profile).
-include("x11_sentinel_server.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

%% Constructor
-export([new/1]).

%% Getters
-export([get_profile_id/1,
         get_user_id/1,
         get_profile_data/1,
         get_succeeded_at/1,
         get_failed_at/1,
         get_created_at/1,
         get_updated_at/1]).

%% Traits
-export([is_succeeded/1,
         is_in_progress/1]).

-export_type([profile_id/0,
              profile_data/0,
              profile/0]).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type profile_id() :: binary().

-type profile_data() :: binary() | undefined.

-type profile() ::
        #{profile_id := profile_id(),

          % The ID of the user this profile belongs to.
          user_id := xss_user:user_id(),

          profile_data := profile_data(),

          % When was the profile successfully built. If `undefined', the profile
          % has not been successfully built.
          succeeded_at := nucleus_time:timestamp() | undefined,

          % When did the profile building fail. If `undefined', the profile
          % building has not failed.
          failed_at := nucleus_time:timestamp() | undefined,

          % When was profile building started (and the profile was saved into
          % the database without the profile data).
          created_at := nucleus_time:timestamp() | undefined,

          % When was the user last modified in the database.
          updated_at := nucleus_time:timestamp() | undefined}.

%%%=============================================================================
%%% API functions
%%%=============================================================================

%%%-----------------------------------------------------------------------------
%%% Constructor
%%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Create a profile.
%% @end
%%------------------------------------------------------------------------------
-spec new(#{profile_id := ProfileId,
            user_id := UserId,
            profile_data => ProfileData,
            succeeded_at => SucceededAt,
            failed_at => FailedAt,
            created_at => CreatedAt,
            updated_at => UpdatedAt}) -> Profile when
      ProfileId :: profile_id(),
      UserId :: gts_user:user_id(),
      ProfileData :: profile_data(),
      SucceededAt :: nucleus_time:timestamp() | undefined,
      FailedAt :: nucleus_time:timestamp() | undefined,
      CreatedAt :: nucleus_time:timestamp() | undefined,
      UpdatedAt ::  nucleus_time:timestamp() | undefined,
      Profile :: profile().
new(#{profile_id := ProfileId,
      user_id := UserId} = Args) ->
    #{profile_id => ProfileId,
      user_id => UserId,
      profile_data => maps:get(profile_data, Args, undefined),
      succeeded_at => maps:get(succeeded_at, Args, undefined),
      failed_at => maps:get(failed_at, Args, undefined),
      created_at => maps:get(created_at, Args, undefined),
      updated_at => maps:get(updated_at, Args, undefined)}.

%%%-----------------------------------------------------------------------------
%%% Getters
%%%-----------------------------------------------------------------------------

%%------------------------------------------------------------------------------
%% @doc Get the profile ID.
%% @end
%%------------------------------------------------------------------------------
-spec get_profile_id(Profile) -> ProfileId when
      Profile :: profile(),
      ProfileId :: profile_id().
get_profile_id(#{profile_id := ProfileId}) ->
    ProfileId.

%%------------------------------------------------------------------------------
%% @doc Get the user ID.
%% @end
%%------------------------------------------------------------------------------
-spec get_user_id(Profile) -> UserId when
      Profile :: profile(),
      UserId :: xss_user:user_id().
get_user_id(#{user_id := UserId}) ->
    UserId.

%%------------------------------------------------------------------------------
%% @doc Get the profile data.
%% @end
%%------------------------------------------------------------------------------
-spec get_profile_data(Profile) -> ProfileData when
      Profile :: profile(),
      ProfileData :: profile_data().
get_profile_data(#{profile_data := ProfileData}) ->
    ProfileData.

%%------------------------------------------------------------------------------
%% @doc Get when building the profile succeeded.
%% @end
%%------------------------------------------------------------------------------
-spec get_succeeded_at(Profile) -> SucceededAt when
      Profile :: profile(),
      SucceededAt :: xss_utils:xss_timestamp() | undefined.
get_succeeded_at(#{succeeded_at := SucceededAt}) ->
    SucceededAt.

%%------------------------------------------------------------------------------
%% @doc Get when building the profile failed.
%% @end
%%------------------------------------------------------------------------------
-spec get_failed_at(Profile) -> FailedAt when
      Profile :: profile(),
      FailedAt :: xss_utils:xss_timestamp() | undefined.
get_failed_at(#{failed_at := FailedAt}) ->
    FailedAt.

%%------------------------------------------------------------------------------
%% @doc Get when building the profile was started.
%% @end
%%------------------------------------------------------------------------------
-spec get_created_at(Profile) -> CreatedAt when
      Profile :: profile(),
      CreatedAt :: xss_utils:xss_timestamp().
get_created_at(#{created_at := CreatedAt}) ->
    CreatedAt.

%%------------------------------------------------------------------------------
%% @doc Get when the profile was updated.
%% @end
%%------------------------------------------------------------------------------
-spec get_updated_at(Profile) -> UpdatedAt when
      Profile :: profile(),
      UpdatedAt :: xss_utils:xss_timestamp().
get_updated_at(#{updated_at := UpdatedAt}) ->
    UpdatedAt.

%%------------------------------------------------------------------------------
%% @doc Return whether the profile building succeeded.
%% @end
%%------------------------------------------------------------------------------
-spec is_succeeded(Profile) -> Boolean when
      Profile :: profile(),
      Boolean :: boolean().
is_succeeded(#{succeeded_at := undefined}) ->
    false;
is_succeeded(_Profile) ->
    true.

%%------------------------------------------------------------------------------
%% @doc Return whether the profile building is in progress.
%% @end
%%------------------------------------------------------------------------------
is_in_progress(_Profile = #{succeeded_at := undefined,
                            failed_at := undefined}) ->
   true;
is_in_progress(_Profile) ->
   false.
