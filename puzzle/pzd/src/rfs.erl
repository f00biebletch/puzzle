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
% @doc A request for schedule entity.
%
% $Id$
%

-module(rfs).

-include("pzd.hrl").

-export([pre_commit/1, post_commit/1,sanitize/1]).
-export([save/1,all/0,select/2,select/1,delete/1,filter/1,update/1]).
-export([derive_release/1, derive_modules/1, from_raw/4]).
-export([describe/0]).

describe() -> "A request for schedule to run a module".

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

derive_release(Raw) ->
    case proplists:get_value(<<"release">>, Raw) of
        undefined -> nothing;
        Rel ->
            case proplists:get_value(<<"manifest">>, Rel) of
                undefined -> nothing;
                Manifest ->
                    case release:filter([{<<"manifest">>, Manifest}]) of
                        {ok, []} -> 
                            {NR} = entity:unmochify(Rel),
                            K = release:save(NR),
                            {ok, R} = release:select(K),
                            R;
                        {ok, [Release|_]} ->
                            Release
                    end
            end
    end.

derive_modules(Raw) ->
    case proplists:get_value(<<"pzm">>, Raw) of
        undefined -> [];
        Pzms ->
            lists:map(
              fun({struct, Pzm}) ->
                      make_pzm(
                        Pzm, 
                        pzm:filter([{<<"name">>, 
                                     proplists:get_value(<<"name">>, Pzm)}])
                       )
              end, Pzms)
    end.

make_pzm(Raw, {ok, []}) ->
    logger:debug("~p:make_pzm([]) Raw: ~p",[?MODULE, Raw]),
    Key = pzm:save(Raw),
    {ok, Pzm} = pzm:select(Key),
    make_pzm(Raw, {ok, [Pzm]});
make_pzm(Raw, {ok, [Pzm|_]}) ->
    [{<<"pzm_uri">>, entity:get_uri(Pzm)},
     {<<"setup">>, proplists:get_value(<<"setup">>, Raw)}].

from_raw(Raw, Release, Mods, User) ->
    logger:debug("~p:from_raw() Raw: ~p",[?MODULE,Raw]),
    [{<<"release_uri">>, entity:get_uri(Release)},
     {<<"pzm">>, {array, Mods}}, {<<"user">>, User}].

    
