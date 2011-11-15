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
% @doc Resource module for tpts.
%
% $Id$
%

-module(tpt_resource).
-export([init/1,content_types_provided/2,allowed_methods/2,delete_resource/2]).
-export([process_post/2,resource_exists/2, encodings_provided/2]).
-export([to_json/2, generate_etag/2, is_authorized/2]).
-export([entity/0]).

-include("pzd.hrl").
-include_lib("webmachine/include/webmachine.hrl").

-define(ENTITY, tpt).

init(A) -> entity_resource:init(A).

content_types_provided(Req,Context) ->
    {[{"application/json",to_json}], 
     Req, Context}.

resource_exists(Req, Context) ->
    entity_resource:resource_exists(?ENTITY, Req, Context).

encodings_provided(Req, Context) ->
    entity_resource:encodings_provided(?ENTITY, Req, Context).

generate_etag(Req, Context) ->
    entity_resource:generate_etag(?ENTITY, Req, Context).

is_authorized(Req, Context) ->
    entity_resource:is_authorized(?ENTITY, Req, Context).

allowed_methods(Req, Context) ->
    {['GET','POST','DELETE'], Req, Context}.

to_json(Req, Context) ->
    entity_resource:to_json(?ENTITY, Req, Context).

delete_resource(Req, Context) ->
    entity_resource:delete_resource(?ENTITY, Req, Context).

process_post(Req, Context) ->
    entity_resource:process_post(?ENTITY, Req, Context).

entity() -> ?ENTITY.
    
