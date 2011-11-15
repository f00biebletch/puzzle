%
% Copyright 2011 Kevin McIntire, Gianluca Filippini
%
% Licensed under the Apache License, Version 2.0 (the "License"); you may not 
% use this file except in compliance with the License. You may obtain a copy 
% of the License at 
%
%    http://www.apache.org/licenses/LICENSE-2.0 
%
% Unless required by applicable law or agreed to in writing, software 
% distributed under the License is distributed on an "AS IS" BASIS, 
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
% See the License for the specific language governing permissions and 
% limitations under the License. 
%
% @doc The repository for builds.
%
% $Id$
%

-module(build_repo).
-include("pzd.hrl").

-behaviour(gen_server).

% FIXIT send this with request?  useful for testing is all
-define(BUILD_MACHINE, "somenode").
-define(FTP_USER, "ftp").
-define(FTP_PASSWD, "deviut").
-define(SCOPE,{local, build_repo}).

-record(state, {machine, user, passwd}).

-export([start_link/0,terminate/2, stop/0]).
-export([init/1, handle_call/3, handle_cast/2,handle_info/2,
        code_change/3]).
-export([pull/2]).

init(_Arg) ->
    process_flag(trap_exit, true),
    logger:debug("~p:init(): starting",[?MODULE]),

    {ok, #state{machine=?BUILD_MACHINE, user=?FTP_USER, passwd=?FTP_PASSWD}}.

start_link() ->
    gen_server:start_link(?SCOPE, ?MODULE, [], []).

terminate(_Reason, _State) ->
    logger:info("~p:terminate()",[?MODULE]),
    ok.

handle_call({pull, ManifestPath, DestDir}, _From, 
            State=#state{machine=Machine,user=User,passwd=Passwd}) ->
    {reply, do_pull(ManifestPath, DestDir, Machine, User, Passwd), State};
handle_call({set_machine, BuildMachine}, _From, State) ->
    {reply, BuildMachine, State#state{machine=BuildMachine}};
handle_call({set_user, User}, _From, State) ->
    {reply, User, State#state{user=User}};
handle_call({set_passwd, Passwd}, _From, State) ->
    {reply, Passwd, State#state{passwd=Passwd}};
handle_call(Msg, _From, State) ->
    {reply, Msg, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Msg, State) ->
    {Msg, normal, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
     {ok, State}.
pull(ManifestPath, DestDir) ->
    gen_server:call(?MODULE, {pull, ManifestPath, DestDir}).
stop() -> gen_server:cast(?MODULE, stop).

do_pull(ManifestPath, DestDir, Machine, User, Passwd) ->
    logger:debug("~p:pull(): From ~p to ~p",
                 [?MODULE,ManifestPath,DestDir]),
    Manifest = manifest:fetch({Machine, User, Passwd},
                              ManifestPath),
    Dest = filename:join([DestDir, ?TST_LOC_BIN]),
    fixit = manifest:parse(Manifest),
    Dest.

