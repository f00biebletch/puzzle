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
% @doc Resource module for describing web services.
%
% $Id$
%

-module(about_resource).
-export([init/1,content_types_provided/2]).
-export([resource_exists/2, allowed_methods/2]).
-export([to_json/2, generate_etag/2]).
-export([entity/0]).

-include("pzd.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init(_A) -> {ok, []}.

content_types_provided(Req,Context) ->
    {[{"application/json",to_json}], Req, Context}.

resource_exists(Req, Context) ->
    {true, Req, Context}.
    
to_json(Req, Context) ->
    {mochijson2:encode(entity:mochify(about:describe())), Req, Context}.

allowed_methods(Req, Context) ->
    {['GET'], Req, Context}.

entity() -> none.
    
generate_etag(Req, Context) ->
    {about:hash(), Req, Context}.
