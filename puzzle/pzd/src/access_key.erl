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
% @doc Access key for authorization.
%
% $Id$
%

-module(access_key).

-export([pre_commit/1, post_commit/1,sanitize/1]).
-export([save/1,all/0,select/2,select/1,delete/1,filter/1,update/1]).
-export([new/2, key/1, key_id/1, name/1, email/1]).
-export([activate/1, deactivate/1, is_active/1]).
-export([compare/2]).
-export([describe/0]).

sanitize(T) -> T.

describe() -> "Keys to authenticate and authorize".

pre_commit(R) -> entity:pre_commit(R).
post_commit(R) -> R.

all() -> entity:all(?MODULE).
delete(K) -> entity:delete(?MODULE, K).
filter(P) -> entity:filter(?MODULE, P).
save(AK) -> 
    Key = entity:save(?MODULE, AK),
    {ok, V} = select(Key),
    update(
      lists:keystore
      (<<"key_id">>, 1, V, {<<"key_id">>, base64:encode(crypto:md5(Key))})
      ),
    Key.

update(Obj) -> entity:update(?MODULE, Obj).
select(K) -> entity:select(?MODULE, K).
select(K, Decode) -> entity:select(?MODULE, K, Decode).

name(Key) -> proplists:get_value(<<"contact_name">>, Key).
email(Key) -> proplists:get_value(<<"contact_email">>, Key).
key_id(Key) -> proplists:get_value(<<"key_id">>, Key).
key(Key) -> base64:decode(proplists:get_value(<<"key">>, Key)).

compare(AccessKey, OtherData) ->
    util:ensure_binary(OtherData) =:= 
        proplists:get_value(<<"key">>, AccessKey).
    
new(Name, Email) ->
    [
     {<<"contact_name">>, util:ensure_binary(Name)},
     {<<"contact_email">>, util:ensure_binary(Email)},
     {<<"key">>, base64:encode(integer_to_list(crypto_util:generate_key()))}
    ].

activate(AK) ->
    entity:add_exec(AK, exec:new(<<"activated">>, <<"ok">>, node())).
deactivate(AK) ->
    entity:add_exec(AK, exec:new(<<"deactivated">>, <<"ok">>, node())).

is_active(AK) ->
    case entity:get_latest_exec(AK) of
        undefined -> false;
        E -> (util:ensure_binary(exec:get_status(E)) =:= <<"activated">>)
    end.

                    
