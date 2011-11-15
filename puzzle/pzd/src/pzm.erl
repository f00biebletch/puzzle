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
% @doc A PZ Module.
%
% $Id$
%

-module(pzm).

-include("pzd.hrl").

-export([pre_commit/1, post_commit/1,sanitize/1]).
-export([save/1,all/0,select/2,select/1,delete/1,filter/1,update/1]).
-export([name/1, repo_path/1]).
-export([describe/0]).

describe() -> "A module is a complete test suite definition".
    
sanitize(N) -> 
    case lists:all(fun(K) -> proplists:is_defined(K,N) end, 
                   [<<"name">>, <<"pzm_path">>]) of
        true -> N;
        _ -> invalid
    end.

pre_commit(H) -> entity:pre_commit(H).
post_commit(H) -> H.

all() -> entity:all(?MODULE).
delete(K) -> entity:delete(?MODULE, K).
filter(P) -> entity:filter(?MODULE, P).
save(Obj) -> entity:save(?MODULE, Obj).
update(Obj) -> entity:update(?MODULE, Obj).
select(K) -> entity:select(?MODULE, K).
select(K, Decode) -> entity:select(?MODULE, K, Decode).

name(Pzm) -> proplists:get_value(<<"name">>, Pzm).
repo_path(Pzm) -> proplists:get_value(<<"pzm_path">>, Pzm).

