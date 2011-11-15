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
% @doc gen_server to run tsts.
%
% $Id$
%
-module(tstomatic).

-behaviour(gen_server).

-export([start_link/0,terminate/2]).
-export([init/1, handle_call/3, handle_cast/2,handle_info/2,
        code_change/3]).
-export([run_tst/1, run_rfs/1]).
-export([cancel_tst/1]).
-export([node_down/1]).
-export([schedule_rfs/1, do_cancel/1]).

-include("pzd.hrl").

-define(SCOPE,{local, tstomatic}).

init(_Arg) ->
    process_flag(trap_exit, true),
    logger:debug("~p:init(): starting",[?MODULE]),

    {ok, []}.

start_link() ->
    gen_server:start_link(?SCOPE, ?MODULE, [], []).

terminate(_Reason, _State) ->
    logger:info("~p:terminate()",[?MODULE]),
    ok.

handle_call(Msg, _From, State) ->
    {reply, Msg, State}.
handle_cast({run_rfs, Rfs}, State) ->
    spawn_link(?MODULE, schedule_rfs, [Rfs]),
    logger:debug("~p:run_rfs(): my work is done here...",[?MODULE]),
    {noreply, State};
handle_cast({run_tst, Tst}, State) ->
    do_run(Tst),
    {noreply, State};
handle_cast({cancel_tst, Tst}, State) ->
    spawn_link(?MODULE, do_cancel, [Tst]),
    {noreply, State};
handle_cast({node_down, Node}, State) ->
    
    Kids = lists:filter(fun is_exec/1,
                        supervisor:which_children(pzd_sup)),
    lists:foreach(fun({_Id, Pid, _Type, _Mods}) -> 
                          gen_server:cast(Pid, {node_down, Node})
                  end,
                  Kids),
    {noreply, State};
handle_cast(Msg, State) ->
    {Msg, normal, State}.

is_exec({_Id, _Child, _Type, Mods}) when is_list(Mods) ->
    hd(Mods) =:= tst_exec;
is_exec(_) -> false.

schedule_rfs(Rfs) ->
    {array, Mods} = proplists:get_value(<<"pzm">>, Rfs),
    Rel = case entity:get_link(proplists:get_value(<<"release_uri">>, Rfs)) of
              undefined -> undefined;
              {ok, R} -> R
          end,
    Tsts = lists:map(
             fun(Mod) ->
                     logger:debug("~p:schedule_rfs(): ~p",
                                  [?MODULE, Mod]),
                     {ok, Pzm} = entity:get_link(
                                   proplists:get_value(<<"pzm_uri">>, Mod)),
                                   % FIXIT need to pass Setup along
                     _Setup = proplists:get_value(<<"setup">>, Mod),
                     tst:for_rfs(Rfs, Rel, Pzm)
             end, Mods),
    
    Rfs2 = entity:add_exec(Rfs, exec:new(<<"started">>, ok, node())),
    rfs:update(Rfs2),
    
    logger:debug("~p:schedule_rfs(~p): ~p",
                 [?MODULE, entity:get_key(Rfs), Mods]),
    schedule_batch(Rfs2, Tsts).

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
     {ok, State}.

run_tst(Tst) -> gen_server:cast(?MODULE, {run_tst, Tst}).
cancel_tst(Tst) -> gen_server:cast(?MODULE, {cancel_tst, Tst}).
run_rfs(Rfs) -> gen_server:cast(?MODULE, {run_rfs, Rfs}).
node_down(Node) -> gen_server:cast(?MODULE, {node_down, Node}).

schedule_batch(_Rfs, Tsts) ->
    lists:foreach(fun(Tst) -> 
                          do_run(Tst) 
                  end, 
                  Tsts).

do_run(Tst) ->
    Key = entity:get_key(Tst),
    logger:debug("~p:run_tst(~p)",[?MODULE, Key]),
    Id = util:ensure_atom(Key),

    {ok, TE} = 
        supervisor:start_child(pzd_sup,
                               {Id,
                                {tst_exec, start_link, [Id]},
                                temporary,
                                10000,
                                worker,
                                [tst_exec]}),

    gen_server:cast(TE, {schedule_tst, Tst}).
    
do_cancel(Tst) ->
    Key = entity:get_key(Tst),
    logger:debug("~p:cancel_tst(~p)",[?MODULE, Key]),
    Id = util:ensure_atom(Key),
    case supervisor:terminate_child(pzd_sup, Id) of
        ok ->
            supervisor:delete_child(pzd_sup, Id);
        Res -> Res
    end.

