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
% @doc Stub for authorization.
%
% $Id$
%

-module(authorization).

-export([validate/3, check_key/2]).

validate(AccessKeyId, Signature, Data) when is_list(Signature) ->
    validate(AccessKeyId, util:ensure_binary(Signature), Data);
validate(AccessKeyId, Signature, Data) ->
    logger:debug("~p:validate() Id = ~p",[?MODULE, AccessKeyId]),
    {ok, Keys} = 
        access_key:filter([
                           {<<"key_id">>, AccessKeyId},
                           {<<"latest_exec.status">>, <<"activated">>}
                          ]),
    case Keys of
        [] -> 
            logger:warn("~p:validate() No valid keys for ~p",
                        [?MODULE, AccessKeyId]),
            {false, undefined};
        [AccessKey|_] ->
            logger:debug("~p:validate() client sig = ~p",[?MODULE, Signature]),
            ServerSign = sign(AccessKey, Data),
            logger:debug("~p:validate() server sig = ~p",[?MODULE, ServerSign]),
            {ServerSign =:= Signature, entity:get_uri(AccessKey)}
    end.

check_key(KeyId, KeyData) ->
    {ok, Keys} = 
        access_key:filter([
                           {<<"key_id">>, KeyId},
                           {<<"latest_exec.status">>, <<"activated">>}
                          ]),
    case Keys of
        [] -> 
            {false, undefined};
        [AccessKey|_] ->
            {access_key:compare(AccessKey, KeyData), entity:get_uri(AccessKey)}
    end.
    
sign(Key, Data) when is_list(Data) ->
    sign(Key, list_to_binary(Data));
sign(Key, Data) ->
    base64:encode(crypto:sha_mac(access_key:key(Key), Data)).



