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
% @doc Resource module for nodes.
%
% $Id$
%

-module(node_resource).
-export([init/1,content_types_provided/2,allowed_methods/2]).
-export([resource_exists/2, process_post/2,encodings_provided/2]).
-export([to_json/2, is_authorized/2]).
-export([entity/0]).

-include("pzd.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-define(ENTITY, node).

init(A) -> entity_resource:init(A).

content_types_provided(Req,Context) ->
    {[{"application/json",to_json}], Req, Context}.

encodings_provided(Req, Context) ->
    entity_resource:encodings_provided(?ENTITY, Req, Context).

resource_exists(Req, Context) ->
    resource_exists(wrq:disp_path(Req), Req, Context).
resource_exists([], Req, Context) ->
    Vals = gen_server:call(cluster, {describe_grouped}, 3000),
    {true, Req, Context#context{key=all, value=Vals}};
resource_exists(Path, Req, Context) ->
    % freaking FIXIT use dispatch better!!!
    Toks = filename:split(Path),
    case Toks of 
        ["status"] ->
            resource_exists("status", [], Req, Context);
        [Key] ->
            Val = gen_server:call(cluster, {get_node, Key}),
            case Val of
                none ->
                    {false, Req, Context#context{key=Key,value=Val}};
                V ->
                    {true, Req, Context#context{key=Key,value=V}}
            end;
        [Action, Id] ->
            resource_exists(Action, Id, Req, Context)
    end.

resource_exists("status", [], Req, Context) ->
    Stats = [ make_status(N) || 
                N <- gen_server:call(cluster, {describe}, 3000)],
    {true, Req, Context#context{key=all, value=Stats}};
resource_exists("status", Id, Req, Context) ->
    Val = gen_server:call(cluster, {get_node, Id}),
    case Val of
        none ->
            {false, Req, Context#context{key=Id,value=Val}};
        _ ->
            {true, Req, Context#context{key=Id,value=make_status(Val)}}
    end.

make_status(Node) ->
    [
     {<<"_id">>, util:ensure_binary(entity:get_key(Node))},
     lists:keyfind(<<"status">>, 1, Node),
     {<<"running_tpts">>, 
      proplists:get_value(<<"running_tpts">>, Node, [])},
     {<<"staging">>, proplists:get_value(<<"staging">>, Node)}
    ].
    
to_json(Req, Context) ->
    entity_resource:to_json(?ENTITY, Req, Context).
    
is_authorized(Req, Context) ->
    entity_resource:is_authorized(?ENTITY, Req, Context).

allowed_methods(Req, Context) ->
    {['GET', 'POST', 'DELETE'], Req, Context}.

process_post(Req, Context) ->
    process_post(wrq:disp_path(Req), Req, Context).

process_post([], Req, Context) ->
    Body = wrq:req_body(Req),
    logger:debug("~p process_post Body: ~p",[?MODULE,Body]),

    {struct, V} = mochijson2:decode(Body),

    Key= node:save(V),
    {ok,Obj} = node:select(Key),

    cluster:add_node(Obj),

    Json = entity:to_json(Obj),

    Req1 = wrq:set_resp_header("Content-Type", "application/json",Req),
    Req2 = wrq:append_to_resp_body(Json, Req1),

    {true,Req2,Context};
process_post("all", Req, Context) ->
    % FIXIT some c/p with next clause
    Body = wrq:req_body(Req),
    logger:debug("~p:process_post(all ) Body: ~p",[?MODULE,Body]),

    {struct, V} = mochijson2:decode(Body),

    Action = util:ensure_atom(
               string:to_lower(
                 util:ensure_list(proplists:get_value(<<"action">>, V)))),
    logger:debug("~p:process_post(all) Action: ~p",[?MODULE,Action]),
    Res = case Action of
        image -> cluster:image_nodes();
        upgrade -> cluster:upgrade_nodes()
    end,
    logger:debug("~p:process_post(all) Res: ~p",[?MODULE,Res]),
    Json = entity:to_json(Res),

    Req1 = wrq:set_resp_header("Content-Type", "application/json",Req),
    Req2 = wrq:append_to_resp_body(Json, Req1),
    {true, Req2, Context};
process_post(Key, Req, Context) ->
    Body = wrq:req_body(Req),
    logger:debug("~p:process_post(~p) Body: ~p",[?MODULE,Key, Body]),

    {struct, V} = mochijson2:decode(Body),

    Action = util:ensure_atom(
               string:to_lower(
                 util:ensure_list(proplists:get_value(<<"action">>, V)))),
    logger:debug("~p:process_post() Action: ~p",[?MODULE,Action]),
    
    % FIXIT move this to an entity perhaps?
    {ok,Obj} = node:select(Key),

    case Action of
        stop -> cluster:stop_node(Obj);
        start -> cluster:start_node(Obj);
        upgrade -> cluster:upgrade_node(Obj);
        _ ->
            logger:info("~p:process_post(): Unknown Action~p",
                         [?MODULE,Action])
    end,

    Json = entity:to_json(Obj),

    Req1 = wrq:set_resp_header("Content-Type", "application/json",Req),
    Req2 = wrq:append_to_resp_body(Json, Req1),
    {true, Req2, Context}.

entity() -> ?ENTITY.

    


