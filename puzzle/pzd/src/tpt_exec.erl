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
% @doc tpt execution in a gen_fsm
%
% $Id$
%
-module(tpt_exec).

-behaviour(gen_fsm).

-include("pzd.hrl").

-export([init/1, unscheduled/2, scheduled/2, staged/2, 
         sent/2, started/2, done/2]).
-export([handle_event/3, handle_sync_event/4, handle_info/3, 
         terminate/3, code_change/4]).

-record(state, {tpt, node}).

init([]) ->
    process_flag(trap_exit, true),    
    {ok, unscheduled, #state{}}.
handle_event({node_down, Node}, StateName, State) ->
    logger:debug("~p:handle_event(~p): state = ~p",
                 [?MODULE, node_down, StateName]),
    Down = util:ensure_atom(proplists:get_value(<<"name">>, Node)),
    Cur = util:ensure_atom(proplists:get_value(<<"name">>, State#state.node)),
    case Down =:= Cur of
        true ->
            terminate({node_down}, StateName, State);
        _ ->
            {next_state, StateName, State}
    end;
handle_event(_Event, StateName, State) ->
    logger:debug("~p:handle_event(~p): state = ~p",
                 [?MODULE, _Event, StateName]),
    {next_state, StateName, State}.
handle_sync_event(_Event, _From, StateName, State) ->
    logger:debug("~p:handle_sync_event(~p): state = ~p",
                 [?MODULE, _Event, StateName]),
    {reply, ok, StateName, State}.
handle_info(_Info, StateName, State) ->
    logger:debug("~p:handle_info(~p): state = ~p",[?MODULE, _Info, StateName]),
    {next_state, StateName, State}.

terminate({shutdown, {_Tpt, _Node}}, _StateName, _State) ->
    ok;
terminate(Reason={node_down}, StateName, State) ->
    logger:info("~p:terminate(~p): State [ ~p ]: ~p",
                [?MODULE, entity:get_key(State#state.tpt), StateName, Reason]),
    tpt_node_down(State);
terminate(Reason, StateName, State=#state{tpt=Tpt, node=Node}) ->
    % trap any other errors and send to supervisor
    logger:info("~p:terminate(~p): State [ ~p ]: ~p",
                [?MODULE, entity:get_key(State#state.tpt), StateName, Reason]),
    cancel_tpt(Node, Tpt),
    tpt_error(State#state.node, State#state.tpt, 
              util:ensure_binary(io_lib:format("~p",[Reason]))).

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%% States %%%%
unscheduled({schedule, Tpt, Node}, State) ->
    T1 = tpt:run(Tpt),
    Key = entity:get_key(T1),
    Name = proplists:get_value(<<"name">>, Node),
    logger:debug("~p:unscheduled(~p): ~p",[?MODULE, Key, Name]),
    Exec = exec:new(scheduled, Name, erlang:node()),
    T2 = entity:add_exec(T1, Exec),
    _T3 = tpt:update(T2),
    {ok,V} = tpt:select(Key),
    {next_state, scheduled, State#state{tpt=V, node=Node}};
unscheduled(_Event, State) ->
    logger:debug("~p:unscheduled() noop ~p",[?MODULE, _Event]),
    {next_state, unscheduled, State}.

scheduled({stage}, State=#state{tpt=Tpt, node=Node}) ->
    [_, Name] = node:parse_name(Node),
    Key = entity:get_key(Tpt),
    Root = tpt:get_fs_root(Tpt),

    tpt:stage(Tpt, Node, Root),

    Exec = exec:new(staged, Name, erlang:node()),
    {ok,T1} = tpt:select(Key),
    T2 = entity:add_exec(T1, Exec),
    _T3 = tpt:update(T2),
    {ok,V} = tpt:select(Key),
    
    {next_state, staged, State#state{tpt=V}};

scheduled(_Event, State) ->
    logger:debug("~p:scheduled() noop ~p",[?MODULE, _Event]),
    {next_state, scheduled, State}.

staged({execute}, State=#state{tpt=Tpt, node=Node}) ->
    [_, Name] = node:parse_name(Node),
    From = self(),
    logger:debug("~p:staged(~p): node -> ~p",
                 [?MODULE, entity:get_key(Tpt), Name]),
    rpc:cast(util:ensure_atom(proplists:get_value(<<"name">>, Node)),
             executor, 
             execute_tpt, 
             [From,  Tpt]),
    {next_state, sent, State};
staged(_Event, State) ->
    logger:debug("~p:staged(): noop ~p",[?MODULE, _Event]),
    {next_state, staged, State}.

sent({tpt_started, NewNode}, State=#state{tpt=Tpt, node=Node}) ->
    [_, Name] = node:parse_name(Node),
    cluster:merge_nodes([NewNode]),
    Exec = exec:new(started, ok, 
                    proplists:get_value(<<"name">>,Node)),

    Key = entity:get_key(Tpt),
    logger:debug("~p:sent(~p): node -> ~p",[?MODULE, Key, Name]), 
    T1 = entity:add_exec(Tpt, Exec),
    tpt:update(T1),
    {ok,V} = tpt:select(Key),
    {next_state, started, State#state{tpt=V}, hibernate};
sent(_Event, State) ->
    % FIXIT maybe this is idle? or hibernate?
    logger:debug("~p:sent(): noop ~p",[?MODULE, _Event]),
    {next_state, sent, State}.

started({tpt_done, Res, NewNode}, State=#state{tpt=Tpt, node=Node}) ->
    logger:debug("~p:tpt_done(~p)",
                 [?MODULE, entity:get_key(Tpt)]), 
    Reason = case Res of
                 % In this case we've received nothing from the node, and
                 % perl has crashed w/o sending anything back.  Bummer.
                 [] -> tpt_error(Node, Tpt, "Unknown Error");
                 _ ->
                     try
                         case entity:from_json(Res) of
                             {_Val, <<"0">>} -> 
                                 tpt_worked(Node, Tpt, Res);
                             {Error, _Code} -> 
                                 tpt_error(Node, Tpt, Error)
                         end
                     catch
                         error:_Reason ->
                             tpt_error(Node, Tpt, Res)
                     end
             end,
    {_, {T1, _}} = Reason,
    cluster:merge_nodes([NewNode]),
    {stop, Reason, State#state{tpt=T1} };
started({tpt_terminated, Res, NewNode}, State=#state{tpt=Tpt}) ->
    logger:debug("~p:tpt_terminated(~p): ~p",
                 [?MODULE, entity:get_key(Tpt), Res]), 
    cluster:merge_nodes([NewNode]),
    tpt_node_down(State);

started({tpt_error, Res, NewNode}, State=#state{tpt=Tpt, node=Node}) ->
    logger:error("~p:tpt_error(~p)",
                 [?MODULE, entity:get_key(Tpt)]), 
    Reason = tpt_error(Node, Tpt, Res),
    {_, {T1, _}} = Reason,
    cluster:merge_nodes([NewNode]),
    {stop, Reason, State#state{tpt=T1} };
started(_Event, State) ->
    logger:debug("~p:started(): noop ~p",[?MODULE, _Event]), 
    {next_state, started, State}.

done(_Event, State) ->
    {stop, normal, State}.

%%% Privates %%%%
tpt_worked(Node, T, Res) ->
    Key = entity:get_key(T),
    logger:debug("~p:tpt_worked(~p): Res -> ~p",[?MODULE,Key,Res]),
    
    T2 = finish_tpt(Node, T),

    {shutdown, 
     {tpt:complete(T2, Res, proplists:get_value(<<"name">>,Node)), Node}}.

tpt_error(Node, T, Error) ->
    Key = entity:get_key(T),
    logger:error("~p:tpt_error(~p) Res -> ~p",[?MODULE,Key,Error]),
    Reason = case util:is_proplist(Error) of
              true -> 
                  Errs =  proplists:get_value(<<"ERROR">>, Error),
                  lists:foldl(fun({_K, Err}, Str) ->
                                      util:ensure_list(Err)++Str
                              end, "", Errs);
              _ -> Error
          end,
    T2 = finish_tpt(Node, T),
    T3 = tpt:errored(T2, Reason, proplists:get_value(<<"name">>, Node)),
    {tpt_error, {T3, Node}}.

tpt_node_down(State=#state{tpt=T,node=Node}) ->
    Key = entity:get_key(T),
    NN = proplists:get_value(<<"name">>, Node),
    logger:error("~p:tpt_error(~p) node_down ~p",[?MODULE,Key, NN]),
    T1 = tpt:errored(T, "Node Down", NN),
    {stop, {tpt_node_down, {T1, Node}}, State#state{tpt=T1} }.

finish_tpt(Node, Tpt) ->
    % FIXIT when finish a tpt, might want to also update the status
    % of the job as well - need better stats!
    logger:debug("~p:tpt_finish_tpt(~p):",[?MODULE,entity:get_key(Tpt)]),
    [_, Name] = node:parse_name(Node),

    Output = result:collect(Name, Tpt),
    Tpt1 = [{<<"result_dir">>, Output}|Tpt],
    tpt:update(Tpt1),  
    Tpt1.
    
cancel_tpt(Node, Tpt) ->
    [_, Name] = node:parse_name(Node),
    logger:debug("~p:cancel_tpt(~p): node -> ~p",
                 [?MODULE, entity:get_key(Tpt), Name]),
    rpc:call(util:ensure_atom(proplists:get_value(<<"name">>, Node)),
                   executor, 
                   cancel_tpt, 
                   [Tpt]).
    


