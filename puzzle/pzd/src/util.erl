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
% @doc leftover code
%
% $Id$

-module(util).
-compile(export_all).

is_proplist([{_,_}|_]) -> true;
is_proplist([]) -> true;
is_proplist(_) -> false.    

% recur by default
make_scp() -> "scp -p -q -r -c blowfish ".
make_scp(no_recur) -> "scp -p -q -c blowfish ".

make_scp(Src, Host, Dest) ->
    make_scp() ++ " "++Src++" "++Host++":"++Dest.
make_scp(Src, Host, Dest, F=no_recur) ->
    make_scp(F) ++ " "++Src++" "++Host++":"++Dest.

% FIXIT what about proplist get access to string?
ensure_binary(X) when is_binary(X) -> X;
ensure_binary(X) when is_list(X) -> list_to_binary(X);
ensure_binary(X) when is_atom(X) -> list_to_binary(atom_to_list(X)).

ensure_list(X) when is_atom(X) -> atom_to_list(X);
ensure_list(X) when is_binary(X) -> binary_to_list(X);
ensure_list(X) when is_list(X) -> X.

ensure_atom(X) when is_atom(X) -> X;
ensure_atom(X) -> list_to_atom(ensure_list(X)).
     
prop_append(P, E, K) ->
    Val = proplists:get_value(K,P),
    case Val of
        undefined ->
            [{K,{array, [E]}}|P];
        {array,[]} ->
            lists:keyreplace(K, 1, P, 
                             {K, {array,[E]}});
        {array, Vals} ->
            lists:keyreplace(K, 1, P, 
                             {K, {array,[E|Vals]}})
    end.

prop_remove(P, E, K) ->
    prop_remove(P, E, K, soft).

prop_remove(P, E, K, hard) ->
    Val = proplists:get_value(K,P),
    case Val of
        undefined ->
            P;
        {array, Vals} ->
            case lists:delete(E, Vals) of
                [] -> lists:keydelete(K, 1, P);
                New -> lists:keyreplace(K, 1, P, {K, {array,New}})
            end
    end;
prop_remove(P, E, K, _) ->
    Val = proplists:get_value(K,P),
    case Val of
        undefined ->
            P;
        {array, Vals} ->
            New = lists:delete(E, Vals),
            lists:keyreplace(K, 1, P, {K, {array,New}})
    end.

prop_update(L, K, V) ->
    Val = proplists:get_value(K,L),
    case Val of
        undefined ->
            L;
        _ ->
            lists:keyreplace(K, 1, L, {K, V})
    end.
    
shuffle(List) -> shuffle(List, []).
 
shuffle([], Acc) -> Acc; 
shuffle(List, Acc) ->
     {Leading, [H | T]} = lists:split(random:uniform(length(List)) - 1, List),
     shuffle(Leading ++ T, [H | Acc]).

diff_seconds(DTNew, DTOld) ->
    calendar:datetime_to_gregorian_seconds(DTNew) - 
        calendar:datetime_to_gregorian_seconds(DTOld).

generate_key(Module, Id) ->
    util:ensure_atom(util:ensure_list(Module)++"_"++util:ensure_list(Id)).
    
sha(Data) -> mochihex:to_hex(ensure_list(crypto:sha(Data))).
    
