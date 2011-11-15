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
% @doc Resource module for rfs.
%
% $Id$
%

-module(rfs_resource).
-export([init/1,content_types_provided/2,allowed_methods/2,delete_resource/2]).
-export([content_types_accepted/2,process_post/2,resource_exists/2]).
-export([to_json/2, is_authorized/2, generate_etag/2]).
-export([entity/0]).

-define(ENTITY, rfs).

-include("pzd.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init(A) -> entity_resource:init(A).

content_types_provided(Req,Context) ->
    {[{"application/json",to_json}], Req, Context}.

resource_exists(Req, Context) ->
    entity_resource:resource_exists(?ENTITY, Req, Context).

to_json(Req, Context) ->
    entity_resource:to_json(?ENTITY, Req, Context).

generate_etag(Req, Context) ->
    entity_resource:generate_etag(?ENTITY, Req, Context).

allowed_methods(Req, Context) ->
    {['GET','POST','DELETE'], Req, Context}.

content_types_accepted(Req, Context) ->  % called when 'PUT' comes
    {[{"application/json", from_json}], Req, Context}.

delete_resource(Req, Context) ->
    entity_resource:delete_resource(?ENTITY, Req, Context).

is_authorized(Req, Context) ->
    entity_resource:is_authorized(?ENTITY, Req, Context).

process_post(Req, Context) ->
    Body = wrq:req_body(Req),
    logger:debug("~p:process_post() Body: ~p",[?MODULE,Body]),

    {struct, Moch} = mochijson2:decode(Body),
    {RFSRaw} = entity:unmochify(Moch),
    logger:debug("~p:process_post() RFS: ~p",[?ENTITY,RFSRaw]),
    
    Rel = rfs:derive_release(RFSRaw),

    Mods = rfs:derive_modules(RFSRaw),
           
    RFS = rfs:from_raw(RFSRaw, Rel, Mods, Context#context.user),
    logger:debug("~p:process_post() RFS: ~p",[?ENTITY,RFS]),
    RFS2 = entity:add_exec(RFS, exec:new(<<"accepted">>, <<"ok">>, node())),

    SK = rfs:save(RFS2),
    {ok, RFS3} = rfs:select(SK),

    tstomatic:run_rfs(RFS3),

    Json = entity:to_json(RFS3),

    Req1 = wrq:set_resp_header("Content-Type", "application/json",Req),
    Req2 = wrq:append_to_resp_body(Json, Req1),

    {true,Req2,Context#context{value=RFS2}}.

entity() -> ?ENTITY.

    
