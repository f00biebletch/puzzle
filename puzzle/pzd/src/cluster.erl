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
% @doc gen_server for node monitor.
%
% $Id$
%
-module(cluster).

-behaviour(gen_server).

-export([all/0, select/1]).
-export([start_link/0,terminate/2]).
-export([init/1, handle_call/3, handle_cast/2,handle_info/2,
        code_change/3]).
-export([add_node/1,stop_node/1,start_node/1,stage_node/2,
        upgrade_node/1]).
-export([image_nodes/0, upgrade_nodes/0]).
-export([get_node/1]).
-export([merge_nodes/1]).

-include("pzd.hrl").

-define(SCOPE,{local, cluster}).
-record(state, {nodes}).
-record(node_shadow, {proxy=nothing, entity=nothing}).

init(_Arg) ->
    process_flag(trap_exit, true),
    logger:debug("~p:init(): starting",[?MODULE]),

    {ok, Nodes} = node:all(),
    
    logger:debug("~p:init(): Nodes = ~p",[?MODULE,Nodes]),
    State = lists:foldl(fun(N, D) ->
                                do_start_node(N, D)
                        end, #state{nodes=dict:new()}, Nodes),
    {ok, State}.

start_link() ->
    gen_server:start_link(?SCOPE, ?MODULE, [], []).

terminate(_Reason, _State) ->
    logger:info("~p:terminate()",[?MODULE]),
    ok.

describe_nodes(Nodes) ->
    nodes_to_list(
      dict:map(fun(_Id, Val) ->
                       build_node(Val)
               end,
               Nodes)
     ).
    
handle_call({get_node, Id}, _From, State) ->
    {reply, build_node(Id, State), State};
handle_call({describe}, _From, State) ->
    {reply, describe_nodes(State#state.nodes), State};
handle_call({describe_grouped}, _From, State) ->
    Nodes = describe_nodes(State#state.nodes),
    Vals = dict:to_list(lists:foldl(fun(N, Dict) -> 
                                            Domain = node:parse_domain(N),
                                            dict:append(Domain, N, Dict)
                                    end, dict:new(), Nodes)),
    {reply, Vals, State};
handle_call({describe, Domain}, _From, State) ->
    Nodes = dict:filter(fun(_Id, Val) ->
                                (node:parse_domain(
                                   Val#node_shadow.entity) =:= Domain)
                        end,
                        State#state.nodes),
    {reply, describe_nodes(Nodes), State};
handle_call({stop_node, Node}, _From, State) ->
    S2 = stop_node(Node, State),
    {reply, ok, S2};
handle_call({add_node, Node}, _From, State) ->
    {reply, ok, do_add_node(Node, State)};
handle_call({start_node, Node}, _From, State) ->
    {reply, ok, do_start_node(Node, State)};
handle_call({merge_nodes, Nodes}, _From, State) ->
    lists:foreach(
      fun(Node) ->
              Id = entity:get_key(Node),
              case dict:find(Id, State#state.nodes) of
                  error -> 
                      ok;
                  {ok, S} ->
                      case S#node_shadow.proxy of
                          nothing ->
                              ok;
                          P ->
                              gen_server:call(P, {merge, Node})
                      end
              end
      end,
      Nodes),
    {reply, ok, State};
handle_call({upgrade_node, Node}, _From, State) ->
    S2 = do_upgrade_node(Node, State),
    {reply, ok, S2};
handle_call({missed_heartbeat, HBs}, _From, State) ->
    S2 = handle_missed_heartbeats([node:for_heartbeat(HB) || HB <- HBs], State),
    {reply, ok, S2};
handle_call(Msg, _From, State) ->
    {reply, Msg, State}.

handle_cast({heartbeat, HB}, State) ->
    {noreply, handle_heartbeat(HB, node:for_heartbeat(HB), State)};
handle_cast({image_nodes}, State) ->
    dict:map(fun(_Id, Val) -> 
                     spawn_link(fun() ->
                                        stage_node(Val#node_shadow.entity, 
                                                   State) end)
             end,
             State#state.nodes),
    {noreply, State};
handle_cast({upgrade_nodes}, State) -> 
    S2 = dict:fold(fun(_Id, Val, St) -> 
                           do_upgrade_node(Val#node_shadow.entity, St)
                   end,
                   State,
                   State#state.nodes),
    {noreply, S2};
handle_cast(Msg, State) ->
    {Msg, normal, State}.

handle_info({'EXIT', Pid, Reason}, State) ->
    % Mark the node as having no proxy, which means down!
    logger:warn("~p:handle_info() EXIT ~p",
                 [?MODULE, Reason]),
    D2 = dict:map(fun(_Id, V) ->
                          case V#node_shadow.proxy =/= Pid of
                              true -> V;
                              _ -> V#node_shadow{proxy=nothing}
                          end
                  end, State#state.nodes),
    {noreply, State#state{nodes = D2}};
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
     {ok, State}.
    
all() ->
    gen_server:call(?MODULE, {describe}).
select(Group) ->
    gen_server:call(?MODULE, {describe, Group}).
stop_node(Node) ->
    gen_server:call(?MODULE, {stop_node, Node}).
add_node(Node) ->
    gen_server:call(?MODULE, {add_node, Node}, 30000).
start_node(Node) ->
    gen_server:call(?MODULE, {start_node, Node}, 30000).
upgrade_node(Node) ->
    gen_server:call(?MODULE, {upgrade_node, Node}, 30000).
image_nodes() ->
    gen_server:cast(?MODULE, {image_nodes}).
upgrade_nodes() ->
    gen_server:cast(?MODULE, {upgrade_nodes}).
get_node(Id) ->
    gen_server:call(?MODULE, {get_node, Id}).
merge_nodes(Nodes) ->
    gen_server:call(?MODULE, {merge_nodes, Nodes}).

stop_node(Node, State) ->
    Id = entity:get_key(Node),
    logger:debug("~p:stop_node(~p)",[?MODULE, Id]),
    Nodes = case dict:find(Id, State#state.nodes) of
                error -> 
                    State#state.nodes;
                {ok, Val} ->
                    gen_server:cast(Val#node_shadow.proxy, {shutdown}),
                    % reset proxy to null as it were
                    dict:store(Id, 
                               Val#node_shadow{proxy=nothing}, 
                               State#state.nodes)
            end,
    State#state{nodes=Nodes}.

do_start_node(Node, State) ->
    Name = util:ensure_list(proplists:get_value(<<"name">>,Node)),    
    logger:debug("~p:do_start_node(): Starting ~p",
                 [?MODULE,Name]),

    {ok, NE} = gen_server:start_link(node_proxy, Node, []),
    logger:debug("~p:do_start_node(~p)",[?MODULE, Name]),
    
    gen_server:cast(NE, {boot}),
    State#state{nodes=
                dict:store(entity:get_key(Node), 
                           #node_shadow{entity=Node, proxy=NE}, 
                           State#state.nodes)}.

do_upgrade_node(Node, State) ->
    S2 = stop_node(Node, State),
    stage_node(Node, S2),
    do_start_node(Node, S2).

do_add_node(Node, State) ->
    stage_node(Node, State),
    do_start_node(Node, State).

stage_node(Node, State) -> 
    Id = entity:get_key(Node),
    logger:debug("~p:stage_node(~p)",[?MODULE, Id]),
    case dict:find(Id, State#state.nodes) of
        error -> 
            [_NN, Host] = node:parse_name(Node),
            logger:debug("~p:stage_node(): Staging ~p", [?MODULE,Host]),
            _Location = node:deploy(Host);
        {ok, Val} ->
            gen_server:cast(Val#node_shadow.proxy, {stage})
    end.

nodes_to_list(Dict) -> [V || {_K,V} <- dict:to_list(Dict)].

build_node(Id, State) ->    
    case dict:find(util:ensure_binary(Id), State#state.nodes) of
        error -> [];
        {ok, V} -> build_node(V)
    end.

build_node(Shadow) ->
    case Shadow#node_shadow.proxy of
        nothing -> node_down(Shadow);
        P -> 
            try
                gen_server:call(P, {describe}, 3000)
            catch
                _Error:_Reason -> node_down(Shadow)
            end
    end.

node_down(Shadow) ->
    [{<<"status">>,down}|Shadow#node_shadow.entity].
    
handle_heartbeat(HB, [], State) ->
    % NOTE node and heartbeat are isomorphic so we are using heartbeat
    % in place of node info
    Val = node:from_heartbeat(HB),
    Key= node:save(Val),
    {ok,Node} = node:select(Key),
    logger:announce("~p:handle_heartbeat() Added Node = ~p",
                    [?MODULE, proplists:get_value(<<"name">>, Node)]),
    do_add_node(Node, State);
handle_heartbeat(HB, Node, State) -> % ignore any others
    Key = entity:get_key(Node),
    
    case dict:find(Key, State#state.nodes) of
        error -> ok;
        {ok, Val} -> gen_server:cast(Val#node_shadow.proxy, {heartbeat, HB})
    end,

    State.

handle_missed_heartbeats([], State) ->
    State;
handle_missed_heartbeats(_Nodes, State) ->
    State.
