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
% @doc gen_server for single execution of a tst.
%
% $Id$
%
-module(tst_exec).

-behaviour(gen_server).

-export([start_link/1, start/1, terminate/2]).
-export([init/1, handle_call/3, handle_cast/2,handle_info/2,
        code_change/3]).

-include("pzd.hrl").
-record(state, {tst, work, workers}).

init(_Arg) ->
    process_flag(trap_exit, true),    
    {ok, #state{tst=nothing, workers=dict:new()}}.

start_link(Id) ->
    start(Id).
start(Id) ->
    gen_server:start_link({local, util:generate_key(?MODULE, Id)}, 
                          ?MODULE, [], []).

terminate(Reason, State) when State#state.tst =:= nothing ->
    logger:info("~p:terminate(~p): ~p",
                [?MODULE, nothing, Reason]),
    ok;
terminate(Reason, State) ->
    logger:info("~p:terminate(~p): ~p",
                [?MODULE, entity:get_key(State#state.tst), Reason]),
    ok.
code_change(_OldVsn, State, _Extra) ->
     {ok, State}.

handle_info({'EXIT', _Pid, normal}, State) ->
    logger:debug("~p:handle_info() EXIT!!!!",
                 [?MODULE]),
    {noreply, State};
handle_info({'EXIT', _Pid, {shutdown, Id}}, State=#state{tst=Tst}) ->
    logger:debug("~p:handle_info(~p) EXIT from ~p",
                 [?MODULE, entity:get_key(Tst), Id]),
    job_done(Id, State);
handle_info({'EXIT', _Pid, {job_error, Id}}, State=#state{tst=Tst}) ->
    logger:error("~p:handle_info(~p) Job Error ~p", 
                 [?MODULE, entity:get_key(Tst), Id]),
    {ok, J} = job:select(Id),
    Priority = proplists:get_value(<<"PRIORITY">>, J),
    clean_jobs(Tst, Priority),
    Tst2 = tst_error(Tst),
    job_done(Id, State#state{tst=Tst2}),
    {noreply, State}.

handle_call(_Any, _From, State) ->
    {reply, none, State}.

% FIXIT this is an odd server - it should only ever be called once!
% hence the function clause state = [].  Maybe is should be a FSM too?
handle_cast({schedule_tst, Tst}, #state{tst=nothing}) ->
    Name = proplists:get_value(<<"NAME">>, Tst),
    logger:announce("~p:schedule_tst(~p) New",
                    [?MODULE, Name]),
    Tst1 = tst:new(Tst),
    Tst2 = tst:workflow(Tst1),
    logger:announce("~p:scheduled_tst(~p) Running ~p",
                    [?MODULE, entity:get_key(Tst2), Name]),
    Tst3 = tst:run(Tst2),
    {ok, Jobs} = job:filter([{"parent_uri", entity:get_uri(Tst3)}]),
    logger:debug("~p:schedule_tst(~p) Got ~p jobs~n",
                 [?MODULE, entity:get_key(Tst3), length(Jobs)]),
    % Group jobs by priority
    Work = 
        lists:keysort(1,
                      lists:foldl(
                        fun(J, Acc) -> 
                                Pri = proplists:get_value(<<"PRIORITY">>, J),
                                util:prop_append(Acc, entity:get_key(J), Pri)
                        end, 
                        [], Jobs)
                     ),
    State = #state{tst=Tst3, work=Work},
    work(State);

handle_cast(stop, State) ->
    logger:debug("~p:stop()",[?MODULE]),
    {stop, normal, State};
handle_cast({node_down, Node}, State) ->
    logger:debug("~p:node_down(~p)",
                 [?MODULE,entity:get_key(State#state.tst)]),
    lists:foreach(fun({_Key, Worker}) -> 
                          gen_server:cast(Worker, {node_down, Node})
                  end,
                  dict:to_list(State#state.workers)),
    {noreply, State};
handle_cast(Other, State) ->
    logger:debug("~p: Other message: ~p",[?MODULE,Other]),
    {noreply, State}.

work(State=#state{tst=Tst, work=[]}) ->
    logger:debug("~p:work(~p): maybe complete tst!", 
                 [?MODULE, entity:get_key(Tst)]),
    case tst:maybe_complete(Tst) of
        {true, Tst2} ->
            logger:debug("~p:work(~p) Finishing tst!", 
                         [?MODULE, entity:get_key(Tst2)]),
            log_completion(Tst2),
            logger:debug("~p:work(~p) finalize tst", 
                         [?MODULE, entity:get_key(Tst2)]),
            tst:finalize(Tst2),
            logger:debug("~p:work(~p) clean tst", 
                         [?MODULE, entity:get_key(Tst2)]),
            tst:clean(Tst),
            logger:announce("~p:work(~p) Finished tst!", 
                            [?MODULE, entity:get_key(Tst2)]),
            {stop, normal, State#state{tst=Tst2}};
        {error, Tst2} -> 
            logger:error("~p:tst_done(~p): errored", 
                         [?MODULE, entity:get_key(Tst2)]),
            tst_error(Tst2),
            {noreply, State#state{tst=Tst2}};
        {false, _} ->
            logger:debug("~p:tst_done(~p): DID NOT complete tst!", 
                         [?MODULE, entity:get_key(Tst)]),
            {noreply, State}
    end;
work(State=#state{tst=Tst, work=[{Priority, {array, Jobs}}|_Rest],
                 workers=Workers}) ->
    logger:debug("~p:start_jobs(~p): starting ~p jobs for priority ~p",
                 [?MODULE, entity:get_key(Tst), length(Jobs), Priority]),
    W2 = case Workers of
             undefined -> dict:new();
             _ -> Workers
         end,
    Nw = lists:foldl(fun(JobKey, Acc) ->
                             JE = execute_job(JobKey),
                             dict:store(JobKey, JE, Acc)
                     end, W2, Jobs),
    {noreply, State#state{workers=Nw}}.
    
execute_job(Key) ->
    {ok, Job} = job:select(Key),
    {ok, JE} = gen_server:start_link(job_exec, [], []),
    logger:debug("~p:execute_job(~p)",[?MODULE, Key]),
    gen_server:cast(JE, {schedule_job, Job}),
    JE.

job_done(JobKey, State=#state{tst=Tst, work=Val}) ->
    [{Priority, _Jobs}|_Rest] = Val,
    logger:debug("~p:job_done(~p): Val = ~p",[?MODULE, JobKey, Val]),
    V2 = util:prop_remove(Val, JobKey, Priority, hard),
    W2 = dict:erase(JobKey, State#state.workers),
    logger:debug("~p:job_done(~p): V2 = ~p",[?MODULE, JobKey, V2]),
    case proplists:is_defined(Priority, V2) of
        false -> 
            logger:debug("~p:job_done(~p): done with priority ~p",
                         [?MODULE, JobKey, Priority]),

            % clean all jobs for this priority - can't do it after
                 % each job in a given priority since those run concurrently
                 % and we will run into FS contention issues!
            clean_jobs(Tst, Priority),

            case priority_advance(Tst, Priority) of
                true ->
                    % Proceed to next priority
                    work(State#state{work=V2, workers=W2});
                _ ->
                    {stop, {tst_error, entity:get_key(Tst)}, 
                            State#state{work=V2, workers=W2}}
            end;
        _ -> 
            logger:debug("~p:job_done(~p): done with job ~p",
                         [?MODULE, JobKey, JobKey]),
            {noreply, State#state{work=V2, workers=W2}}
         end.

% Clean all jobs for the given priority in this tst.
% Avoids FS contention by cleaning after all concurrent jobs are done
clean_jobs(Tst, Priority) ->
    {ok, Jobs} = job:filter([
                             {"parent_uri", entity:get_uri(Tst)},
                             {"PRIORITY", Priority}
                            ]),
    % FIXIT with 100s of tpts, we really need to clean a node only once!
    lists:foreach(fun(J) -> 
                          logger:debug("~p:clean_jobs(~p): cleaning ~p",
                                       [?MODULE, Priority, entity:get_key(J)]),
                          job:clean(J)
                  end,
                  Jobs).

priority_advance(Tst, Priority) ->
    {ok, Jobs} = job:filter([
                             {"parent_uri", entity:get_uri(Tst)},
                             {"PRIORITY", Priority}
                            ]),
    lists:all(fun(J) -> entity:is_complete(J) end, Jobs).

log_completion(Tst) ->
    write_to_log(Tst, "FINISH").

write_to_log(Tst, Msg) ->
    LogName = tst:get_log_file(Tst),
    {ok, FD} = file:open(LogName, [append]),
    Str = "Run , "++Msg++" , "++
        httpd_util:rfc1123_date(erlang:localtime())++"~n",
    io:fwrite(FD, Str, []),
    file:close(FD).
    
tst_error(Tst) ->
    tst:clean(Tst),
    Exec = exec:new(error, "Tst Failed", 
                    erlang:node()),

    Tst2 = entity:add_exec(Tst, Exec),
    tst:update(Tst2),
    {ok,Tst3} = tst:select(entity:get_key(Tst2)),
    Tst3.
