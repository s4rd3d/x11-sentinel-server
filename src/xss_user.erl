%%%-----------------------------------------------------------------------------
%%% @doc The user model represents a user of the `x11-sentinel-client'
%%%      application.
%%% @end
%%%-----------------------------------------------------------------------------

-module(xss_user).
-include("x11_sentinel_server.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

-export([new/1,
         get_user_id/1,
         get_event_count/1,
         get_created_at/1,
         get_updated_at/1]).

-export_type([user/0,
              user_id/0]).

%%%=============================================================================
%%% Types
%%%=============================================================================

%% Primary identifier of the user.
-type user_id() :: binary().

%% Unix time stamp.
-type timestamp() :: integer().

-type user() :: #{user_id := user_id(),

                  % The nunber of events sent by this user to the server. It is
                  % set to 'undefined' when enough event is present for building
                  % a biometric profile.
                  event_count := non_neg_integer() | undefined,

                  % When was the user added to the database.
                  created_at := timestamp() | undefined,

                  % When was the user last modified.
                  updated_at := timestamp() | undefined}.

%%%=============================================================================
%%% Exported functions
%%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Create a new user.
%% @end
%%-----------------------------------------------------------------------------
-spec new(#{user_id := UserId,
            event_count => EventCount,
            created_at => CreatedAt,
            updated_at => UpdatedAt}) -> User when
      UserId :: user_id(),
      EventCount :: non_neg_integer() | undefined,
      CreatedAt :: timestamp() | undefined,
      UpdatedAt :: timestamp() | undefined,
      User :: user().
new(#{user_id := UserId} = User) ->
    #{user_id => UserId,
      event_count => maps:get(event_count, User, 0),
      created_at => maps:get(created_at, User, undefined),
      updated_at => maps:get(updated_at, User, undefined)}.

%%-----------------------------------------------------------------------------
%% @doc Get the `user_id' field of the user.
%% @end
%%-----------------------------------------------------------------------------
-spec get_user_id(User) -> UserId when
      User :: user(),
      UserId :: user_id().
get_user_id(#{user_id := UserId}) ->
    UserId.

%%-----------------------------------------------------------------------------
%% @doc Get the `event_count' field of the user.
%% @end
%%-----------------------------------------------------------------------------
-spec get_event_count(User) -> EventCount when
      User :: user(),
      EventCount :: non_neg_integer() | undefined.
get_event_count(#{event_count := EventCount}) ->
    EventCount.

%%-----------------------------------------------------------------------------
%% @doc Get the `created_at' field of the user.
%% @end
%%-----------------------------------------------------------------------------
-spec get_created_at(User) -> CreatedAt when
      User :: user(),
      CreatedAt :: timestamp() | undefined.
get_created_at(#{created_at := CreatedAt}) ->
    CreatedAt.

%%-----------------------------------------------------------------------------
%% @doc Get the `updated_at' field of the user.
%% @end
%%-----------------------------------------------------------------------------
-spec get_updated_at(User) -> UpdatedAt when
      User :: user(),
      UpdatedAt :: timestamp() | undefined.
get_updated_at(#{updated_at := UpdatedAt}) ->
    UpdatedAt.
