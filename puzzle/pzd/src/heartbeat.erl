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
% @doc A hearbeat entity.
%
% $Id$
%

-module(heartbeat).

-include("pzd.hrl").

-export([pre_commit/1, post_commit/1,sanitize/1]).
-export([save/1,all/0,select/2,select/1,delete/1,filter/1,update/1]).
-export([reconcile/2, hostname/1]).
-export([describe/0]).

describe() -> "An OS level heartbeat from a compute node".
    
sanitize(H) -> H.
pre_commit(H) -> entity:pre_commit(H).
post_commit(H) -> H.

all() -> entity:all(?MODULE).
delete(K) -> entity:delete(?MODULE, K).
filter(P) -> entity:filter(?MODULE, P).
save(Obj) -> entity:save(?MODULE, Obj).
update(Obj) -> entity:update(?MODULE, Obj).
select(K) -> entity:select(?MODULE, K).
select(K, Decode) -> entity:select(?MODULE, K, Decode).

reconcile(New, Old) ->
    Val = entity:pre_commit(New), % update timestamp
    K = proplists:get_value(<<"_id">>, Old),
    [{<<"_id">>, K}|Val]. % set key for update
    
hostname(HB) ->
    proplists:get_value(<<"NAME">>,
                        proplists:get_value(<<"HOST">>, HB)).
    
