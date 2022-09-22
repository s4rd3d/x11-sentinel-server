%%%-----------------------------------------------------------------------------
%%% @doc The session model represents a x11 session of the `x11-sentinel-client'
%%%      application.
%%% @end
%%%-----------------------------------------------------------------------------

-module(xss_session).
-include("x11_sentinel_server.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

-export([new/1,
         get_session_id/1,
         get_created_at/1,
         get_updated_at/1]).

-export_type([session/0,
              session_id/0]).

%%%=============================================================================
%%% Types
%%%=============================================================================

%% Primary identifier of the session.
-type session_id() :: binary().

-type session() :: #{session_id := session_id(),
                     % When was the session added to the database.
                     created_at := xss_utils:xss_timestamp() | undefined,
                     % When was the session last modified.
                     updated_at := xss_utils:xss_timestamp() | undefined}.

%%%=============================================================================
%%% Exported functions
%%%=============================================================================

%%-----------------------------------------------------------------------------
%% @doc Create a new session.
%% @end
%%-----------------------------------------------------------------------------
-spec new(#{session_id := SessionId,
            created_at => CreatedAt,
            updated_at => UpdatedAt}) -> Session when
      SessionId :: session_id(),
      CreatedAt :: xss_utils:xss_timestamp() | undefined,
      UpdatedAt :: xss_utils:xss_timestamp() | undefined,
      Session :: session().
new(#{session_id := SessionId} = Session) ->
    #{session_id => SessionId,
      created_at => maps:get(created_at, Session, undefined),
      updated_at => maps:get(updated_at, Session, undefined)}.

%%-----------------------------------------------------------------------------
%% @doc Get the `session_id' field of the session.
%% @end
%%-----------------------------------------------------------------------------
-spec get_session_id(Session) -> SessionId when
      Session :: session(),
      SessionId :: session_id().
get_session_id(#{session_id := SessionId}) ->
    SessionId.

%%-----------------------------------------------------------------------------
%% @doc Get the `created_at' field of the session.
%% @end
%%-----------------------------------------------------------------------------
-spec get_created_at(Session) -> CreatedAt when
      Session :: session(),
      CreatedAt :: xss_utils:xss_timestamp() | undefined.
get_created_at(#{created_at := CreatedAt}) ->
    CreatedAt.

%%-----------------------------------------------------------------------------
%% @doc Get the `updated_at' field of the session.
%% @end
%%-----------------------------------------------------------------------------
-spec get_updated_at(Session) -> UpdatedAt when
      Session :: session(),
      UpdatedAt :: xss_utils:xss_timestamp() | undefined.
get_updated_at(#{updated_at := UpdatedAt}) ->
    UpdatedAt.
