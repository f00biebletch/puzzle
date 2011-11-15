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
% @doc Entity representing a compute node.
%
% $Id$
%

-module(node).

-include("pzd.hrl").

-export([pre_commit/1, post_commit/1,sanitize/1]).
-export([save/1,all/0,select/2,select/1,delete/1,filter/1,filter/2,update/1]).
-export([parse_name/1, parse_domain/1,deploy/1,revert/1]).
-export([supports_hw/2]).
-export([merge/2, from_heartbeat/1, for_heartbeat/1]).
-export([maybe_run/1]).
-export([describe/0]).

-define(CODE_SOURCE, "/usr/local/lib/pzd/ndexe").
-define(CODE_DEST, "./ndexe").
-define(BACKUP_TAG, ".bak").

describe() -> "A compute node within the cluster".

sanitize(N) -> 
    Fld = fun(K) -> 
                  proplists:is_defined(K,N)
          end,
    case lists:all(Fld, [<<"name">>])
        of
        true ->
            N;
        _ ->
            invalid
    end.

pre_commit(T) -> entity:pre_commit(T).
post_commit(T) -> T.

all() -> entity:all(?MODULE).
delete(K) -> entity:delete(?MODULE, K).
filter(P) -> entity:filter(?MODULE, P).
filter(P, S) -> entity:filter(?MODULE, P, S).
save(Obj) -> entity:save(?MODULE, Obj).
update(Obj) -> entity:update(?MODULE, Obj).
select(K) -> entity:select(?MODULE, K).
select(K, Decode) -> entity:select(?MODULE, K, Decode).

for_heartbeat(HB) ->
    NN = util:ensure_list(heartbeat:hostname(HB)),
    {ok, Nodes} = node:filter([{"name",{regex, NN++"\$","i"}}]),
    case Nodes of
        [] -> [];
        _ ->
            1 = length(Nodes),
            hd(Nodes)
    end.
    
parse_name(N) ->
    Name = util:ensure_list(proplists:get_value(<<"name">>,N)),
    string:tokens(Name,"@").
    
parse_domain(N) ->
    [_NN, Host] = node:parse_name(N),
    string:strip(string:substr(Host,string:str(Host,".")), left, $.).

make_backup_name(Dest) ->
    string:strip(util:ensure_list(Dest), right, $/)++?BACKUP_TAG.
    
backup(Host) ->
    Cmd = "ssh "++?PZD_USER++"@"++Host++" 'cp -rf "++?CODE_DEST++
        " "++make_backup_name(?CODE_DEST)++"'",
    Bak = os:cmd(Cmd),
    logger:debug("~p:backup(): ~p -> ~p",[?MODULE, Cmd, Bak]),
    backed_up.

revert(Host) ->
    Cmd = "ssh "++?PZD_USER++"@"++Host++" 'cp -rf "++
        make_backup_name(?CODE_DEST)++" "++?CODE_DEST++"'",
    Revert = os:cmd(Cmd),
    logger:debug("~p:revert(): ~p -> ~p",[?MODULE, Cmd, Revert]),
    reverted.

deploy(Host) ->
    Src = ?CODE_SOURCE,
    Dest = ?CODE_DEST,

    backed_up = backup(Host),

    C1 = "ssh "++Host++" \"mkdir "++Dest++"\"",
    os:cmd(C1),
    % FIXIT really just need to deploy beam and config - need to
    % figure out deployment model better
    Cmd = util:make_scp()++" "++filename:join([Src,"*"])++
        " "++?PZD_USER++"@"++Host++":"++Dest,
    Scp = os:cmd(Cmd),
    logger:debug("~p:deploy(): ~p -> ~p",[?MODULE, Cmd, Scp]),
    Dest.
 
% need to make node stuff a little more coherent
supports_hw(Tpt, Node) ->
    case util:ensure_binary(proplists:get_value(<<"status">>, Node)) of
        <<"down">> -> false;
        _ -> do_supports_hw(Tpt, Node)
    end.

do_supports_hw(Tpt, Node) ->
    Hw = proplists:get_value(<<"HW">>, Tpt),
    Os = proplists:get_value(<<"OS">>, Tpt),
    Brand = binary_to_list(proplists:get_value(<<"BRAND">>, Os)),
    Bit = proplists:get_value(<<"BIT">>, Os),
    Inst = binary_to_list(proplists:get_value(<<"ISET">>, Hw)),
    Arch = binary_to_list(proplists:get_value(<<"ARCH">>, Hw)),
    % FIXIT need to be able to do OS version <=>, CPU <=>
    Support = [ is_same_os(Brand, Node), 
                supports_instruction(Inst, Node),
                supports_arch(Arch, Node),
                supports_bit_size(Bit, Node),
                supports_cpu(proplists:get_value(<<"CPU">>, Hw), Node)
              ],
    %logger:debug("Node ~p: support = ~p",[entity:get_key(Node), Support]),
    lists:all(fun(V) -> V end, Support).

is_same_os(Os, Node) when is_binary(Os) -> 
    is_same_os(binary_to_list(Os), Node);
is_same_os(Req, Node) ->
    Los = string:to_lower(Req),
    Lios = 
        string:to_lower(
          binary_to_list(
            proplists:get_value(<<"BRAND">>,
                                proplists:get_value(<<"OS">>, Node)))),
    ((string:str(Lios, Los) > 0) or (string:str(Los, Lios) > 0)).

supports_instruction(Instr, Node) when is_binary(Instr) ->
    supports_instruction(binary_to_list(Instr), Node);
supports_instruction(Instr, Node) ->
    Ins = proplists:get_value(<<"instr_set">>, Node),
    Linstr = string:to_lower(Instr),
    supports_instr(Linstr, Ins).
supports_instr("any", _Instr) -> true;
supports_instr(Req, Instr) ->
    lists:member(Req, [string:to_lower(binary_to_list(I)) || I <- Instr]).
    
supports_arch(Arch, Node) when is_binary(Arch) ->
    supports_arch(binary_to_list(Arch), Node);
supports_arch("any", _Node) -> true;
supports_arch(Arch, Node) ->
    Ar = 
        binary_to_list(
          proplists:get_value(<<"ARCH">>, 
                              proplists:get_value(<<"HW">>, Node))),
    Arch =:= Ar.

supports_bit_size("any", _Node) -> true;
supports_bit_size(Bit, Node) when is_binary(Bit) ->
    supports_arch(binary_to_list(Bit), Node);
supports_bit_size(Bit, Node) when is_list(Bit) ->
    supports_arch(list_to_integer(Bit), Node);
supports_bit_size(Bit, Node) ->
    Bit =:= list_to_integer(util:ensure_list(
                              proplists:get_value(
                                <<"BIT">>, 
                                proplists:get_value(<<"OS">>, Node)))).

supports_cpu(ReqCpu, Node) ->
    
    Hw = proplists:get_value(<<"CPU">>, proplists:get_value(<<"HW">>, Node)),
    My = [ string:to_lower(binary_to_list(proplists:get_value(V, Hw))) ||
             V <- [<<"NAME">>,<<"CODENAME">>] ],
    Req = [ string:to_lower(binary_to_list(proplists:get_value(V, ReqCpu))) ||
              V <- [<<"NAME">>,<<"CODENAME">>] ],
    lists:all(fun(T) -> T end, 
              [ (element(2,V) =:= "any") or (element(1,V) =:= element(2,V)) || 
                  V <- lists:zip(
                         My, 
                         Req
                        )]).

from_heartbeat(HB) ->   
    Full = util:ensure_list(
             proplists:get_value(
               <<"NAME">>, proplists:get_value(<<"HOST">>, HB))),
    Idx = string:str(Full, ".")-1,
    Host = case Idx of
               -1 -> Full;
               _ -> string:strip(string:substr(Full,1,Idx), left, $.)
           end,
    Name = util:ensure_list(Host)++"@"++Full,
    Val = [{<<"name">>, Name}|entity:strip(HB)],
    [{<<"code_path">>, ?CODE_DEST}|Val].
    
% Node and heartbeat are isomorphic; this reconciles 2 instances
merge(Node, HB) ->
    lists:ukeymerge(
      1, 
      lists:keysort(1, entity:strip(HB)), 
      lists:keysort(1, Node)
     ).

% FIXIT shared with ndexe! 
get_max_concurrent_jobs(Node) ->
    get_concurrent(proplists:get_value(<<"max_concurrent_jobs">>, Node)).

get_concurrent(B) when is_binary(B)->
    get_concurrent(binary_to_list(B));
get_concurrent(B) when is_list(B) ->
    get_concurrent(list_to_integer(B));
get_concurrent(B) when is_float(B) ->
    trunc(B);
get_concurrent(B) when is_integer(B) ->
    B;
get_concurrent(_B) -> 
    1.

maybe_run(Node) ->
    Running = proplists:get_value(<<"running_tpts">>, Node),
    maybe_run(Node, Running).

maybe_run(Node, undefined) ->
    maybe_run(Node, []);
maybe_run(Node, {array, []}) ->
    maybe_run(Node, []);
maybe_run(_Node, []) ->
    true;
maybe_run(Node, {array,Running}) ->
    length(Running) < get_max_concurrent_jobs(Node).
