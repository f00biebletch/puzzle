%
% Copyright 2011 Kevin McIntire, Gianluca Filippini
% All Rights Reserved
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
% @doc gen_server for single execution of a tpt on a single node.
%
% $Id$
%
-module(tpt_run).

-behaviour(gen_server).

-export([start_link/1,start/1,terminate/2]).
-export([init/1, handle_call/3, handle_cast/2,handle_info/2,
        code_change/3]).
-export([do_execute/4]).
-record(state, {tpt, worker, parent, from}).

init(_Arg) ->
    process_flag(trap_exit, true),    
    {ok, #state{tpt=nothing, worker=nothing, parent=nothing}}.

start_link(Id) ->
    start(Id).
start(Id) ->
    gen_server:start_link({local, make_name(Id)}, ?MODULE, [], []).

make_name(Id) ->
    list_to_atom(atom_to_list(?MODULE)++"_"++atom_to_list(Id)).

terminate(Reason, State) ->
    {oid, Key} = proplists:get_value(<<"_id">>, State#state.tpt),    
    logger:info("~p:terminate(~p): ~p",
                [?MODULE, Key, Reason]),
    case State#state.worker of
        nothing -> ok;
        _ ->
            gen_server:cast(State#state.parent, 
                            {tpt_terminated, 
                             State#state.from, 
                             State#state.tpt, 
                             Reason}),
            erlang:exit(State#state.worker, kill)
    end.

handle_info(Info, State) ->
    logger:debug("~p:handle_info() ~p",[?MODULE, Info]),
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_call(_Any, _From, State) ->
    {reply, none, State}.
    
handle_cast({execute_tpt, Parent, From, Tpt, IsCygwin}, _State) ->

    Pid = spawn_link(?MODULE, do_execute, [Parent, From, Tpt, IsCygwin]),
    
    {noreply, #state{tpt=Tpt, worker=Pid, parent=Parent, from=From}};


handle_cast(stop, State) ->
    logger:debug("~p:stop()",[?MODULE]),
    {stop, normal, State};

handle_cast(Other, State) ->
    logger:debug("~p: Other message: ~p",[?MODULE,Other]),
    {noreply, State}.

make_cmd(Cmd, false) -> Cmd;
make_cmd(Cmd, _) -> "c:\\cygwin\\bin\\bash -c '"++Cmd++"'".

do_execute(Parent, From, Tpt, IsCygwin) ->
    {oid, Key} = proplists:get_value(<<"_id">>, Tpt),
    try
        Scr = 
            binary_to_list(
              proplists:get_value(<<"script_path">>, Tpt)),
        Cmd = Scr ++ " -a run",
        Cmd2 = make_cmd(Cmd, IsCygwin),
        logger:debug("~p:execute_tpt(~p): Running ~p",
                     [?MODULE, Key, Cmd2]),
        Res = os:cmd(Cmd2),
        logger:debug("~p:execute_tpt(~p): Ran tpt, got ~p",
                     [?MODULE, Key, Res]),
        gen_server:cast(Parent, {tpt_done, From, Tpt, Res})
    catch 
        error:_Reason ->
            Err = erlang:get_stacktrace(),
            Msg = io_lib:format("~p",[Err]),
            logger:error(Msg),
            error_logger:error_msg(Msg),
            gen_server:cast(Parent, 
                            {tpt_error, From, Tpt, Err})
    end.
