%%
%% @doc Callbacks for the puzzle application.
%% @copyright 2011 Kevin McIntire, Gianluca Filippini
%%
%% $Id$
%%

-module(ndexe).
-export([start/1, stop/0]).

%% @spec start() -> ok
%% @doc Start the ndexe server.
start(NodeState) ->
    ndexe_deps:ensure(),
    Res = application:start(ndexe),
    executor:initialize(NodeState),
    Res.

%% @spec stop() -> ok
%% @doc Stop the ndexe server.
stop() ->
    application:stop(ndexe).

