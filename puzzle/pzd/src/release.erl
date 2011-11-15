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
% @doc A release.
%
% $Id$
%

-module(release).

-include("pzd.hrl").

-export([pre_commit/1, post_commit/1,sanitize/1]).
-export([save/1,all/0,select/2,select/1,delete/1,filter/1,update/1]).
-export([resolve/2]).
-export([initialize/1]).
-export([describe/0]).

-define(DEFAULT_TEST_MODE, false).

describe() -> "A release of an application".
    
sanitize(T) -> 
    Fld = fun(K) -> 
                  proplists:is_defined(K,T)
          end,
    case lists:all(Fld, [<<"manifest">>,<<"version">>]) of
        true -> T;
        _ -> invalid
    end.

pre_commit(R) -> entity:pre_commit(R).
post_commit(R) -> R.

all() -> entity:all(?MODULE).
delete(K) -> entity:delete(?MODULE, K).
filter(P) -> entity:filter(?MODULE, P).
save(Obj) -> entity:save(?MODULE, Obj).
update(Obj) -> entity:update(?MODULE, Obj).
select(K) -> entity:select(?MODULE, K).
select(K, Decode) -> entity:select(?MODULE, K, Decode).

manifest(Release) -> proplists:get_value(<<"manifest">>, Release).
version(Release) -> proplists:get_value(<<"version">>, Release).
    
initialize(Release) ->
    ok.
    %% Manifest = build_repo:fetch_manifest(release:manifest(Release)),
    %% parse_manifest(Manifest).

parse_manifest(_Manifest) ->
    % FIXIT return all the release info
    ok.

resolve(undefined, _Location) -> resolved;
resolve([], _Location) -> resolved;
resolve(Release, Location) ->
    %% Manifest = util:ensure_list(manifest(Release)),
    %% Version = util:ensure_list(version(Release)),
    %% logger:debug("~p:resolve(): Manifest = ~p, Version = ~p",
    %%              [?MODULE,Manifest,Version]),
    %% {ok, BuildRepo} = build_repo:start_link(),
    %% _Dest = gen_server:call(BuildRepo, {pull,Manifest, Location}),
    %% build_repo:stop(),
    resolved.
    
