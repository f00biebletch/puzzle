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
% @doc Scheduler for nodes.
%
% $Id$
%
-module(ndsched).

%-export([schedule_batch/1]).
-compile(export_all).

-include("pzd.hrl").

schedule_batch(Tpts) ->
    {S2, R2, _B2} = lists:foldl(fun(Tpt, {Scheduled, Rest, Busy}) ->
                                       case maybe_schedule_tpt(Tpt, Busy) of
                                           {none, Busy} -> 
                                               {Scheduled, 
                                                [entity:get_key(Tpt)|Rest], 
                                                Busy};
                                           {Node, Busy2} -> 
                                               {[{Node,Tpt}|Scheduled], 
                                                Rest, Busy2}
                                       end
                               end,                               
                               {[], [], []},
                               Tpts),
    {{scheduled, S2}, {rest, R2}}.

maybe_schedule_tpt(Tpt, Busy) ->
    Nodes = 
        find_nodes(
          proplists:get_value(<<"TYPE">>, 
                              proplists:get_value(<<"CLUSTER">>, Tpt)),
         Tpt, Busy),

    case poll(Nodes, Tpt) of
        false -> 
            {none, Busy};
        Node ->
            {Node, [get_key(Node)|Busy]}
    end.

find_nodes(<<"any">>, Tpt, Busy) ->
    find_nodes(<<"local">>, Tpt, Busy);
find_nodes(<<"hw">>, _Tpt, Busy) ->
    filter_nodes(hw, Busy);
find_nodes(<<"local">>, Tpt, Busy) ->
    % local grid only supports linux 64,
    % so if the tpt needs 32 or not linux,
    % need to delegate to hw cluster
    case cluster_exceptions(Tpt) of
        false -> filter_nodes(local, Busy);
        true -> filter_nodes(hw, Busy)
    end;
find_nodes(_,_,_) ->
    you_are_screwed_fixit.
    
cluster_exceptions(Tpt) ->
    Os = proplists:get_value(<<"OS">>, Tpt),
    Brand = 
        string:to_lower(util:ensure_list(proplists:get_value(<<"BRAND">>, Os))),
    Bit = proplists:get_value(<<"BIT">>, Os),
    lists:any(fun(V) -> V end, [ Brand =:= "windows", Bit =:= 32]).

filter_nodes(Target=hw, Busy) ->
    Nodes = cluster:select(get_subdomain(Target)),
    Flt = 
        fun(Node) ->
                Key = get_key(Node),
                lists:all(fun(NKey) -> Key =/= NKey end, Busy)
        end,

    % filter out nodes on the busy list
    lists:filter(Flt, Nodes);

filter_nodes(Target, _Busy) ->
    cluster:select(get_subdomain(Target)).
      
get_subdomain(hw) -> ?HW_DOMAIN;
get_subdomain(local) -> ?LOCAL_DOMAIN.

poll([], _Tpt) ->
    false;
poll(Nodes, Tpt) ->
    % find nodes that match hw requirement of tpt and that are NOT
    % busy
    Cans = [ util:ensure_atom(proplists:get_value(<<"name">>, Node)) || 
               Node <- Nodes, 
               node:supports_hw(Tpt, Node),
               node:maybe_run(Node)
           ],
    {Defs, _Bads} = rpc:multicall(Cans, executor, request_tpt, [Tpt]),
    % update cluster with defs!
    Finals = [ Node || {true, Node} <- lists:filter(fun find_it/1, Defs)],
    cluster:merge_nodes(Finals),
    pick_best(Finals).

pick_best([]) -> false;
pick_best(Candidates) -> hd(util:shuffle(Candidates)).

find_it({true, _Info}) -> true;
find_it(_) -> false.

% FIXIT guard hack
get_key(Node) ->
    case proplists:get_value(<<"_id">>, Node) of
        {oid, Key} -> Key;
        K -> K
    end.
            
