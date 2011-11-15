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
% @doc Resource module for heartbeats.
%
% $Id$
%

-module(heartbeat_resource).
-export([init/1,content_types_provided/2,allowed_methods/2,
         content_types_accepted/2]).
-export([resource_exists/2, process_post/2]).
-export([generate_etag/2, to_json/2]).
-export([entity/0]).

-include("pzd.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-define(ENTITY, heartbeat).

init(A) -> entity_resource:init(A).

% FIXIT is this a entity_resource?
content_types_provided(Req,Context) ->
    {[{"application/json",to_json}], Req, Context}.

resource_exists(Req, Context) ->
    entity_resource:resource_exists(?ENTITY, Req, Context).
    
to_json(Req, Context) ->
    entity_resource:to_json(?ENTITY, Req, Context).
    
generate_etag(Req, Context) ->
    entity_resource:generate_etag(?ENTITY, Req, Context).

allowed_methods(Req, Context) ->
    {['GET', 'POST'], Req, Context}.

content_types_accepted(Req, Context) ->  % called when 'PUT' comes
    {[{"application/json", from_json}], Req, Context}.

process_post(Req, Context) ->
    process_post(wrq:disp_path(Req), Req, Context).

process_post([], Req, Context) ->
    Body = wrq:req_body(Req),

    {struct, Heartbeat} = mochijson2:decode(Body),
    
    % FIXIT why in tuple??
    {UnHB} = entity:unmochify(Heartbeat),

    HostName = heartbeat:hostname(UnHB),
    {ok, Cur} = heartbeat:filter([{"HOST.NAME", util:ensure_list(HostName)}]),
    Key = case Cur of
              [] ->
                  heartbeat:save(UnHB);
              _ ->
                  Val = heartbeat:reconcile(UnHB, hd(Cur)),
                  heartbeat:update(Val),
                  entity:get_key(Val)
          end,

    {ok,HB} = heartbeat:select(Key),

    gen_server:cast(hbmonitor, {post_heartbeat, HB}),

    Json = entity:to_json(HB),

    Req1 = wrq:set_resp_header("Content-Type", "application/json",Req),
    Req2 = wrq:append_to_resp_body(Json, Req1),

    {true,Req2,Context#context{value=HB}};

process_post(_Key, Req, Context) ->
    {false, Req, Context}.

entity() -> ?ENTITY.

    
