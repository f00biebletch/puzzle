%
% Copyright (c) 2011 Kevin McIntire, Gianluca Filippini
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
% @doc gen_server for managing compute node.
%
% $Id$
%
-module(executor).

-behaviour(gen_server).
-export([start_link/0, terminate/2]).
-export([init/1, handle_call/3, handle_cast/2,handle_info/2,
        code_change/3]).
-export([stage_tpt/1]).
-export([describe/0,request_tpt/1,execute_tpt/2, cancel_tpt/1, initialize/1]).

-define(SCOPE,{local, executor}).

init(_Arg) ->
    process_flag(trap_exit, true),

    S1 = nodeinfo:get_info(),
    S2 = [{<<"running_tpts">>,{array,[]}}|S1],
    {ok, S2}.

start_link() ->
    gen_server:start_link(?SCOPE, ?MODULE,
                          [], []).

terminate(Reason, State) ->
    logger:debug("~p:terminate(): ~p",[?MODULE, Reason]),
    lists:foldl(fun(Uri, S) -> 
                        {S2, _Res} = do_cancel(Uri, S),
                        S2
                end,
                State, 
                flatten(proplists:get_value(<<"running_tpts">>, State))),
                  
    logger:debug("~p:terminate(): Done!",[?MODULE]).

handle_call({describe}, _From, State) ->
    {reply, State, State};

handle_call({initialize, NodeState}, _From, State) ->
    logger:debug("~p:initialize()",[?MODULE]), 
   {reply, ok, lists:ukeymerge(1, NodeState, State)};

handle_call({request_tpt, Tpt}, _From, State) ->
    Decision = maybe_run(State, Tpt),
    {reply, {Decision, State}, State};
handle_call({stage_tpt, Tpt}, _Fromm, State) ->
    case proplists:is_defined(<<"staging">>, State) of
        true -> {reply, false, State};
        false ->
            Uri = proplists:get_value(<<"uri">>, Tpt),
            {reply, true, [{<<"staging">>, Uri}|State]}
    end;
handle_call({cancel_tpt, Tpt}, _From, State) ->
    {S2, Res} = do_cancel(proplists:get_value(<<"uri">>, Tpt), State),
    {reply, Res, S2}.

do_cancel(Uri, State) ->
    logger:debug("~p:cancel_tpt(~p)",[?MODULE, Uri]),
    [_Module, Key] = string:tokens(binary_to_list(Uri), "/"),
    Id = list_to_atom(Key),
    S2 = remove_running(State, Uri),
    S3 = maybe_remove_staging(S2, Uri),
    case supervisor:terminate_child(executor_sup, Id) of
              ok ->
                  {S3, supervisor:delete_child(executor_sup, Id)};
              X -> {S3, X}
    end.
    
execute_tpt(From, Tpt) ->     
    gen_server:cast(?MODULE, {execute_tpt, From, Tpt}).

cancel_tpt(Tpt) ->     
    gen_server:call(?MODULE, {cancel_tpt, Tpt}).

describe() ->
    gen_server:call(?MODULE, {describe}).

request_tpt(Tpt) ->
    gen_server:call(?MODULE, {request_tpt, Tpt}).

stage_tpt(Tpt) ->
    gen_server:call(?MODULE, {stage_tpt, Tpt}).

initialize(NodeState) ->
    gen_server:call(?MODULE, {initialize, NodeState}).

handle_cast({execute_tpt, From, Data}, State) ->
    
    {oid, Key} = proplists:get_value(<<"_id">>, Data),
    % Tell pzd we have started the tpt
    S2 = remove_staging(State),
    S3 = add_running(S2, proplists:get_value(<<"uri">>, Data)),
    Ret = gen_fsm:send_event(From, {tpt_started, S3}),
    logger:debug("~p:execute_tpt(~p) to ~p got ~p",[?MODULE, Key, From, Ret]),

    Id = list_to_atom(binary_to_list(Key)),
    {ok, TE} = 
        supervisor:start_child(executor_sup,
                               {Id,
                                {tpt_run, start_link, 
                                 [Id]},
                                temporary,
                                10000,
                                worker,
                                [tpt_run]}),
    gen_server:cast(TE, {execute_tpt, self(), From, Data, 
                         nodeinfo:is_cygwin(State)}),

    {noreply, S3};
% FIXIT these are similar 
handle_cast({tpt_done, Pid, T, []}, State) ->
    gen_server:cast(?MODULE, 
                    {tpt_error, Pid, T, "No response from script"}),
    {noreply, State};
handle_cast({tpt_done, Pid, T, Res}, State) ->
    {oid, Key} = proplists:get_value(<<"_id">>, T),
    logger:debug("~p:tpt_done(~p): local: ~p",[?MODULE, Key, Res]),
    S2 = remove_running(State, proplists:get_value(<<"uri">>, T)),
    gen_fsm:send_event(Pid, {tpt_done, Res, S2}),
    {noreply, S2};

handle_cast({tpt_terminated, Pid, T, Res}, State) ->
    {oid, Key} = proplists:get_value(<<"_id">>, T),
    logger:debug("~p:tpt_terminated(~p): local: ~p",[?MODULE, Key, Res]),
    S2 = remove_running(State, proplists:get_value(<<"uri">>, T)),
    gen_fsm:send_event(Pid, {tpt_terminated, Res, S2}),
    {noreply, S2};

handle_cast({tpt_error, Pid, T, Res}, State) ->
    {oid, Key} = proplists:get_value(<<"_id">>, T),
    logger:debug("~p:tpt_error(~p): local: ~p",[?MODULE, Key, Res]),
    S2 = remove_running(State, proplists:get_value(<<"uri">>, T)),
    gen_fsm:send_event(Pid, {tpt_error, Res, S2}),
    {noreply, S2};

%% @spec handle_cast(stop, State) -> {stop, normal, State}
%% @doc application terminate callback from async cast.
handle_cast(stop, State) ->
    io:format("OMG like executor server stopping in handle_cast~n"),
    {stop, normal, State}.

%% @spec handle_info(Info, State) -> {noreply, State}
%% @doc handle info callback.
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @doc Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
     {ok, State}.

flatten(undefined) -> [];
flatten({array, X}) -> X.
    
% FIXIT when do we get a maybe???
maybe_run(State, Tpt) ->
    Running = proplists:get_value(<<"running_tpts">>, State),
    maybe_run(State, Tpt, flatten(Running)).
maybe_run(_State, _Tpt, []) ->
    true;
maybe_run(State, _Tpt, Running) ->
    length(Running) < get_max_concurrent_jobs(State).

get_max_concurrent_jobs(State) ->
    get_concurrent(proplists:get_value(<<"max_concurrent_jobs">>, State)).
get_concurrent(B) when is_binary(B)->
    get_concurrent(binary_to_list(B));
get_concurrent(B) when is_list(B) ->
    get_concurrent(list_to_integer(B));
get_concurrent(B) when is_float(B) ->
    trunc(B);
get_concurrent(B) when is_integer(B) ->
    B;
get_concurrent(_B) -> 
    1.

add_running(State, Uri) -> 
    append(State, Uri, <<"running_tpts">>).
remove_running(State, Uri) -> 
    remove(State, Uri, <<"running_tpts">>).
maybe_remove_staging(State, Uri) ->    
    Staging = proplists:get_value(<<"staging">>, State),
    case Uri =:= Staging of
        true -> remove_staging(State);
        _ -> State
    end.
remove_staging(State) ->
    lists:keydelete(<<"staging">>, 1, State).

% FIXIT like exec:add_exec
append(P, E, K) ->
    case proplists:get_value(K,P) of
        undefined -> [{K,{array, [E]}}|P];
        {array,[]} -> lists:keyreplace(K, 1, P, {K, {array,[E]}});
        V ->
            {array, Vals} = V,
            lists:keyreplace(K, 1, P, {K, {array,[E|Vals]}})
    end.

remove(P, E, K) ->
    case proplists:get_value(K,P) of
        undefined -> P;
        V ->
            {array, Vals} = V,
            New = lists:delete(E, Vals),
            lists:keyreplace(K, 1, P, {K, {array,New}})
    end.

