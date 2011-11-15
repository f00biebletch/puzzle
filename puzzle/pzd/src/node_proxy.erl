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
% @doc Node proxy gen_server.
%
% $Id$
%

-module(node_proxy).

-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2,handle_info/2,
        code_change/3,terminate/2]).

-include("pzd.hrl").

-define(SCOPE,{local, node_proxy}).
-define(INTERVAL, 10000).
-record(state, {entity, cur, handler, status, result}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(Entity) ->
    process_flag(trap_exit, true),

    NN = util:ensure_list(proplists:get_value(<<"name">>, Entity)),
    logger:debug("~p:init(~p)",[?MODULE, NN]),
    timer:send_interval(interval_milliseconds(), interval),

    {ok, #state{entity=Entity,cur=none,handler=none,status=down}}.

interval_milliseconds()-> 10000.

handle_info(interval, State)->
    {noreply, ping(State)};
handle_info(_Msg, State) ->
    {noreply, State}.
code_change(_OldVsn, State, _Extra) ->
     {ok, State}.
terminate(_Reason, _State) ->
    ok.

make_node(State=#state{cur=none}) -> [{<<"status">>,starting}|
                                         State#state.entity];
make_node(State) -> 
    X = lists:ukeymerge(1, State#state.cur, State#state.entity),
    S = {<<"status">>, State#state.status},
    case proplists:is_defined(<<"status">>, X) of
        true -> lists:keyreplace(<<"status">>, 1, X, S);
        false -> [S|X]
    end.

handle_call({describe}, _From, State) ->
    Node = make_node(State),
    {reply, Node, State};
handle_call({merge, Node}, _From, State) ->
    {reply, Node, State#state{cur=Node}};
handle_call(Msg, _From, State) ->
    {reply, Msg, State}.

handle_cast({boot}, State) ->
    logger:debug("~p:boot()",[?MODULE]),
    {Pid, Res} = start_app(ndexe, State#state.entity),
    {noreply, State#state{handler=Pid, status=running, result=Res}};
handle_cast({stage}, State) ->
    logger:debug("~p:stage()",[?MODULE]),
    [_NN, Host] = node:parse_name(State#state.entity),
    logger:debug("~p:stage(): Staging ~p", [?MODULE,Host]),
    _Location = node:deploy(Host),

    {noreply, State#state{status=staging}};
handle_cast({shutdown}, State) ->
    logger:debug("~p:shutdown()",[?MODULE]),
    stop_node(State#state.handler, State#state.entity),
    {stop, normal, State};
handle_cast({heartbeat, HB}, State) ->
    Entity = node:merge(State#state.entity, HB),

    node:update(Entity), % save changes!

    {noreply, State#state{entity=Entity}};
handle_cast(Msg, State) ->
    {Msg, normal, State}.
    
start_app(App, Props) ->
    [Node, Host] = node:parse_name(Props),
    start_ndexe(App, Node, Host, Props).

start_ndexe(App, Node, Host, Props) ->
    CodePath = util:ensure_list(proplists:get_value(<<"code_path">>,Props)),
    Cmd = "-rsh ssh -connect_all false -setcookie " ++ 
        util:ensure_list(erlang:get_cookie()) ++ 
        " -pa " ++ filename:join([CodePath, "ebin"]) ++
        " -pa " ++ filename:join([CodePath, "deps", "*", "ebin"]) ++
        " -boot start_sasl",
    logger:debug("~p:start_ndexe(~p): Command: ~p",
                 [?MODULE, Host, Cmd]),

    % FIXIT should be atom erl unless osx
    Erl = "/usr/libexec/StartupItemContext erl",
    %Erl = erl,
    NN = case slave:start(Host, Node, Cmd, no_link, Erl) of
             {ok, N} -> N;
             {error, {already_running, N}} -> N;
             {error, Reason} -> throw({error, Reason})
         end,
    logger:debug("~p:start_ndexe(~p): slave:start = ~p",
                [?MODULE, Host, NN]),
    Pid = spawn(fun() -> keep_alive(App, Host, Node, Props, NN, true) 
                end),
    logger:debug("~p:start_ndexe(~p): Starting ~p",
                [?MODULE, Host, App]),
    
    X = rpc:call(NN, ndexe, start, [Props]),
    logger:debug("~p:start_ndexe(~p) --> ~p for ~p",[?MODULE, Host, X, Cmd]),
    {Pid, X}.

keep_alive(App, Host, Node, Props, NN, Monitor) ->
    case Monitor of
        true -> monitor_node(NN, true);
        false -> ok
    end,
    receive
        {nodedown, _N} ->
            logger:warn("~p:keep_alive(~p): Node down: ~p",
                        [?MODULE, Host, App]),
            tstomatic:node_down(Props),
            start_ndexe(App, Node, Host, Props);
        stop_monitor ->
            logger:debug("~p:keep_alive(~p): stopping monitoring",
                         [?MODULE,Host]),
            tstomatic:node_down(Props),
            monitor_node(NN, false);
        _Other ->
            keep_alive(App, Host, Node, Props, NN, false)
    end.

stop_node(Handler, Nd) ->
    Node = util:ensure_atom(proplists:get_value(<<"name">>, Nd)),
    logger:info("~p:stop_node(~p)",[?MODULE,Node]),
    X = rpc:call(Node, ndexe, stop, []),
    logger:debug("~p:stop_node(~p): stopped ndexe -> ~p",[?MODULE,Node, X]),
    Handler ! stop_monitor,
    slave:stop(Node).

ping(State) ->
    case State#state.cur of
        undefined ->
            do_ping([], State);
        none ->
            do_ping([], State);
        Cur ->
            IsRunning = proplists:get_value(<<"running_tpts">>, Cur),
            do_ping(IsRunning, State)
    end.
    
do_ping(undefined, State) ->
    do_ping([], State);
do_ping(X=[], State) ->
    do_ping({array, X}, State);
do_ping({array, []}, State) ->
    NN = util:ensure_atom(proplists:get_value(<<"name">>, State#state.entity)),
    case rpc:call(NN, executor, describe, []) of
        {badrpc, Reason} ->
            throw({error, Reason});
        Result ->
            State#state{cur=Result}
    end;
do_ping(_A, State) -> 
    State.
    
    
