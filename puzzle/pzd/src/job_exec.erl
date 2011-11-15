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
% @doc gen_server for single execution of a job or part of a job.
%
% $Id$
%
-module(job_exec).

-behaviour(gen_server).

-export([terminate/2]).
-export([init/1, handle_call/3, handle_cast/2,handle_info/2,
        code_change/3]).
-export([work_list/0]).

-include("pzd.hrl").

-record(state, {job, tpts, workers}).

init(_Arg) ->
    process_flag(trap_exit, true),    
    {ok, #state{job=nothing, tpts=[], workers=dict:new()}}.

terminate(Reason, State) ->
%    logger:info("~p:terminate(~p): ~p",
%                [?MODULE, entity:get_key(State#state.job), Reason]),
    ok.
handle_info(interval, State)->
    {noreply, scan_work(State)};
handle_info({'EXIT', _Pid, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', _Pid, {shutdown, {Tpt, Node}}}, State) ->
    logger:debug("~p:handle_info(~p) tpt done",
                 [?MODULE, entity:get_key(Tpt)]),
    tpt_done(Tpt, Node, State);
handle_info({'EXIT', _Pid, {tpt_error, {Tpt, Node}}}, State) ->
    logger:error("~p:handle_info(~p) Job Error", 
                 [?MODULE, entity:get_key(Tpt)]),
    tpt_done(Tpt, Node, State);
handle_info({'EXIT', _Pid, {tpt_node_down, {Tpt, Node}}}, State) ->
    NN = proplists:get_value(<<"name">>, Node),
    logger:error("~p:handle_info(~p) node_down: ~p", 
                 [?MODULE, entity:get_key(Tpt), NN]),
    logger:info("~p:handle_info(~p) NODE DOWN: Rescheduling tpt", 
                [?MODULE, entity:get_key(Tpt)]),
    {noreply, State#state{tpts=[entity:get_key(Tpt)|State#state.tpts]}};
handle_info({'EXIT', _Pid, Reason}, State=#state{job=Job}) ->
    R2  = util:ensure_binary(io_lib:format("~p",[Reason])),
    logger:error("~p:handle_info(~p) Job Error: ~p", 
                 [?MODULE, entity:get_key(Job), R2]),
    J2 = job_error(Job, R2),
    log_error(J2),
    {noreply, State#state{job=J2}};
handle_info(Info, State) ->
    logger:debug("~p:handle_info() ~p",[?MODULE, Info]),
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

work_list() ->
    gen_server:call(?MODULE, {work_list}).

handle_call({work_list}, _From, State=#state{tpts=Tpts}) ->
    {reply, Tpts, State};
handle_call(_Any, _From, State) ->
    {reply, none, State}.

% FIXIT this is an odd server - it should only ever be called once!
% hence the function clause state = [].  Maybe is should be a FSM too?
handle_cast({schedule_job, Job}, #state{job=nothing}) ->
    J1 = job:workflow(Job),
    J2 = job:run(J1),
    log_start(J2),
    {ok, Tpts} = tpt:filter([
                             {"parent_uri", entity:get_uri(J2)},
                             {"latest_exec.status", "info"}
                            ]),
    logger:debug("~p:schedule_job(~p) Got ~p tpts~n",
                 [?MODULE, entity:get_key(J2), length(Tpts)]),
    timer:send_interval(interval_milliseconds(), interval),
    {noreply, #state{job=J2, tpts=[entity:get_key(T) || T <- Tpts]}};

handle_cast({schedule_tpt, Tpt}, #state{job=nothing}) ->
    logger:debug("~p:schedule_tpt(~p)",
                 [?MODULE,entity:get_key(Tpt)]),
    timer:send_interval(interval_milliseconds(), interval),
    {noreply, #state{job=none,tpts=[entity:get_key(Tpt)]}};

handle_cast(stop, State) ->
    logger:debug("~p:stop()",[?MODULE]),
    {stop, normal, State};

handle_cast({node_down, Node}, State) ->
    logger:debug("~p:node_down(~p)",
                 [?MODULE,entity:get_key(State#state.job)]),
    lists:foreach(fun({_Key, Worker}) -> 
                          logger:debug("~p:node_down(~p) calling ~p",
                                       [?MODULE,
                                        entity:get_key(State#state.job),
                                       Worker]),
                          gen_fsm:send_all_state_event(Worker, 
                                                       {node_down, Node})
                  end,
                  dict:to_list(State#state.workers)),    
    {noreply, State};
handle_cast(Other, State) ->
    logger:debug("~p: Other message: ~p",[?MODULE,Other]),
    {noreply, State}.

tpt_done(Tpt, Node, State=#state{job=Job, tpts=[]}) ->
    logger:debug("~p:tpt_done(~p): maybe complete job on ~p", 
                 [?MODULE, entity:get_key(Job), entity:get_key(Tpt)]),
    W2 = dict:erase(entity:get_key(Tpt), State#state.workers),
    case job:maybe_complete(Job) of
        {true, J2} ->
            logger:debug("~p:tpt_done(~p): completed job!", 
                         [?MODULE, entity:get_key(Job)]),
            log_completion(J2),
            {stop, {shutdown, entity:get_key(J2)}, 
             State#state{job=J2, workers=W2}};
        {error, J2} ->
            logger:error("~p:tpt_done(~p): errored job!", 
                         [?MODULE, entity:get_key(Job)]),
            J3 = job_error(J2, Tpt, Node),
            log_error(J3),
            {stop, {job_error, entity:get_key(J2)}, 
             State#state{job=J2, workers=W2}};
        {false, _} ->
            logger:debug("~p:tpt_done(~p): DID NOT complete job!", 
                         [?MODULE, entity:get_key(Job)]),
            {noreply, State#state{workers=W2}}
    end;
tpt_done(Tpt, _Node, State=#state{job=Job}) ->
    logger:debug("~p:tpt_done(~p): not checking ~p since still have work",
                 [?MODULE, entity:get_key(Job), entity:get_key(Tpt)]),
    {noreply, State#state{workers=dict:erase(entity:get_key(Tpt), 
                                             State#state.workers)}}.

log_start(Job) ->
    write_to_log(Job, "START").    
log_completion(Job) ->
    write_to_log(Job, "FINISH").
log_error(Job) ->
    write_to_log(Job, "FAILED").

write_to_log(Job, Msg) ->
    LogName = job:get_log_file(Job),
    {ok, FD} = file:open(LogName, [append]),
    Str = "Run , "++Msg++" , "++
        httpd_util:rfc1123_date(erlang:localtime())++"~n",
    io:fwrite(FD, Str, []),
    file:close(FD).
    
scan_work(S=#state{tpts=ok}) ->
    S;
scan_work(S=#state{tpts=[]}) ->
    S;
scan_work(S=#state{job=Job, tpts=Tpts}) ->
    % FIXIT asking N nodes each about T test points, N*T calls
    % could pass all Tpts to each node, that would be expensive too.
    % currently this just rebuilds the state

    % FIXIT loading tppts here?
    Objs = lists:map(fun(Key) ->
                             {ok, T} = tpt:select(Key),
                             T
                     end, Tpts),
    {{scheduled, Scheduled}, {rest, Rest}} = ndsched:schedule_batch(Objs),

    {NotSched, Workers} = lists:foldl(fun({Node, Tpt}, Acc) ->
                                              do_it(Acc, Node, Tpt)
                                      end,
                {Rest, S#state.workers}, Scheduled),

    S#state{
      job=Job, 
      tpts=NotSched,
      workers=Workers
     }.

do_it({Unscheds, undefined}, Node, Tpt) ->
    do_it({Unscheds, dict:new()}, Node, Tpt);
do_it({Unscheds, Workers}, Node, Tpt) ->
    case maybe_execute_tpt(Node, Tpt) of
        false ->
            {[entity:get_key(Tpt)|Unscheds], Workers};
        TE ->
            {Unscheds, dict:store(entity:get_key(Tpt), TE, Workers)}
    end.

maybe_execute_tpt(Node, Tpt) ->
    case rpc:call(util:ensure_atom(
                    proplists:get_value(<<"name">>, Node)),
                  executor, stage_tpt, [Tpt]) of
        true ->
            {ok, TE} = gen_fsm:start_link(tpt_exec, [], []),
            gen_fsm:send_event(TE, {schedule, Tpt, Node}),
            gen_fsm:send_event(TE, {stage}),
            gen_fsm:send_event(TE, {execute}),
            TE;
        X -> X
    end.

interval_milliseconds()-> 2000.

job_error(Job, Tpt, Node) ->
    job:errored(Job, util:ensure_list(entity:get_key(Tpt)), 
                proplists:get_value(<<"name">>,Node)).

job_error(Job, Reason) ->
    job:errored(Job, Reason).
