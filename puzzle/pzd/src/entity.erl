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
% @doc A generic entity in the system.
%
% $Id$
%

-module(entity).

-define(EXECS,<<"execs">>).
-define(ID,<<"_id">>).

-export([add_exec/2, set_value/3, set_parent_uri/2]).
-export([get_key/1,get_latest_exec/1,get_parent_uri/1,get_uri/1]).
-export([save/2,all/1,filter/2,filter/3,select/2,delete/2,update/2]).

-export([make_cmd/2, mochify/1, unmochify/1,from_json/1, to_json/1]).
-export([pre_commit/1]).
-export([errored/3, errored/4]).

-export([is_complete/1, is_finalized/1, is_error/1, can_cancel/1]).

-export([info/2,info/3,new/2,workflow/2,run/2,finalize/2,complete/2,cancel/3]).
-export([complete/4]).
-export([get_parent/1, get_link/1]).
-export([strip/1]).

-export([hash/1, summary/2, summary/3]).

summary(M, Param) ->
    summary(M, 
           Param, 
           [{"NAME",1}, 
            {"uri",1},
            {"latest_exec.status",1}, 
            {"latest_exec.timestamp",1}]).

summary(M, Param, Fields) ->
    filter(M, Param, Fields).

hash(all) -> undefined;
hash(Obj) ->
    case get_latest_exec(Obj) of
        undefined -> undefined;
        Exec -> base64:encode(exec:get_timestamp(Exec))
    end.
            
make_cmd(Path, Arg) when is_binary(Path) ->
    make_cmd(util:ensure_list(Path), Arg);
make_cmd(Path, Arg) ->
    Path ++" "++Arg.

pre_commit(Obj) ->
    [{<<"added">>,httpd_util:rfc1123_date(erlang:localtime())}|Obj].

from_json(Json) ->
    Moch = mochijson2:decode(Json),
    {Raw} = entity:unmochify(Moch),

    Value = proplists:get_value(<<"VALUE">>, Raw),
    Status = proplists:get_value(<<"STATUS">>, Raw),
    {Value, Status}.

to_json(Obj) ->
    mochijson2:encode(entity:mochify(Obj)).
    
mochify(L=[{_,_}|_]) -> {struct, [mochify(Pair) ||Pair<-L]};
mochify(L) when is_list(L) -> [mochify(X) || X <- L];
mochify({K,{array,V}}) -> {K, [mochify(X) || X <- V]};
mochify({K,{oid,V}}) -> {K, V};
mochify({K, V}) -> {K, mochify(V)};
mochify(Other) -> Other.

unmochify({struct, V}) -> unmochify(V);
unmochify(L=[{_,_}|_]) -> {[unmochify(Pair) ||Pair<-L]};
unmochify(L) when is_list(L) -> {[unmochify(Pair) ||Pair<-L]};
unmochify({K,{struct,V}}) -> {K, [unmochify(X) || X<-V]};
unmochify(Other) -> Other.
    
get_uri(Obj) ->
    proplists:get_value(<<"uri">>, Obj).

get_parent_uri(Obj) ->
    proplists:get_value(<<"parent_uri">>, Obj).

get_key(Obj) -> 
    {oid, Key} = proplists:get_value(<<"_id">>, Obj),
    Key.

make_uri(M, Obj) -> 
    "/"++atom_to_list(M)++"/"++util:ensure_list(get_key(Obj)).

set_parent_uri(Obj, ParentUri) ->
    entity:set_value(<<"parent_uri">>, ParentUri, Obj).

set_value(Key, Val, Obj) ->
    case proplists:is_defined(Key, Obj) of
        true -> Obj;
        _ -> [{Key, Val}|Obj]
    end.
    
add_exec(T, Exec) ->
    K = <<"latest_exec">>,
    Old = proplists:get_value(K, T),
    case Old of
        undefined ->
            [{K, Exec}|T];
        _ ->
            T1 = lists:keyreplace(K, 1, T, {K, Exec}),
            util:prop_append(T1, Old, ?EXECS)
    end.

get_latest_exec(T) ->
    proplists:get_value(<<"latest_exec">>, T).

%% @spec save(atom(), object()) -> object()
%% @doc Save the given object to presistent storage.
save(M,V) ->
    Val0 = M:sanitize(V),
    Val1 = M:pre_commit(Val0),

    % FIXIT db name!
    Mong = mongoapi:new(def, <<"pzd">>),
    {oid, Key} = Mong:save(atom_to_list(M),Val1),

    {ok,Val2} = M:select(Key),
    Val3 = [{<<"uri">>, make_uri(M, Val2)}|Val2],
    update(M, Val3),

    % FIXIT really need to select again to do this, not sure needed
    %Val3 = M:post_commit(Val2),
    Key.

update(M,V) ->
    Key = proplists:get_value(<<"_id">>, V),
    do_update(M, V, Key).

do_update(_M, _V, undefined) -> false;
do_update(_M, _V, <<>>) -> false;
do_update(M, V, Key) ->
    Mong = mongoapi:new(def, <<"pzd">>),
    % FIXIT what to do with result?
    _Res = Mong:update(atom_to_list(M), [{"_id",{oid, Key} }], V, []),
    V.

all(M) ->
    % FIXIT assumption!
    filter(M, [{"uri", {exists, true}}]).

select(M, {oid, Key}) -> select(M, Key);
select(M, Key) when is_binary(Key) ->
    Mong = mongoapi:new(def, <<"pzd">>),
    Mong:findOne(atom_to_list(M), [{"_id",
                                    {oid, Key}
                                   }]);
select(M, Key) -> select(M, list_to_binary(Key)).

filter(M, Param) -> filter(M, Param, undefined).

filter(M, Param, Selector) ->
    Mong = mongoapi:new(def, <<"pzd">>),
    Mong:find(atom_to_list(M), Param, Selector, 0, 9999999).

%% @spec delete(io_list()) -> tpt()
%% @doc Delete the given tpt from presistent storage.
delete(M, Key) when is_binary(Key) ->
    Mong = mongoapi:new(def, <<"pzd">>),
    Res = Mong:remove(atom_to_list(M), [{"_id",
                                         {oid, Key}
                                        }]),
    Res;
delete(M, Key) -> delete(M, list_to_binary(Key)).

info(M, Obj) -> info(M, Obj, "/").
info(_M, Obj=[{_,_}|_],_ParentUri) -> Obj;
info(M, Path,ParentUri) ->
    logger:debug("~p:info(~p)",[M, Path]),
    Cmd = M:make_cmd(Path, "-a info"),
    logger:debug("~p:info(~p) running ~p",[M, Path, Cmd]),
    Info = os:cmd(Cmd),

    {Obj, <<"0">>} = from_json(Info), % check status value

    Exec = exec:new(info, ok, erlang:node()),
    Obj1 = add_exec(Obj,Exec),
    Obj2 = [{<<"script_path">>, Path}|Obj1],
    Obj3 = set_parent_uri(Obj2, ParentUri),

    Key= M:save(Obj3),    
    {ok,V} = M:select(Key),
    logger:debug("~p:info(~p): created ~p",[M, Path, Key]),
    V.

new(M, Obj) ->
    Key = entity:get_key(Obj),

    Path = proplists:get_value(<<"script_path">>, Obj),
    Cmd = M:make_cmd(Path, "-a new"),
    logger:debug("~p:new(~p) Running ~p",[M, Key, Cmd]),    
    New = os:cmd(Cmd),

    Raw = from_json(New),

    Exec = script_result(new, Raw, erlang:node()),
    Obj2 = add_exec(Obj, Exec),

    _Obj3 = M:update(Obj2),

    {_, <<"0">>} = Raw, % check status value

    logger:debug("~p:new(~p): done",[M,Key]),
    {ok,V} = M:select(Key),
    V.

script_result(Action, {_Obj, <<"0">>}, Node) ->
    exec:new(Action, ok, Node);
script_result(Action, {Error, _Code}, Node) ->
    Reasons = proplists:get_value(<<"ERROR">>, Error),
    % just flatten the darn thing out
    Fixit = string:join([util:ensure_list(K)++" "++
                         util:ensure_list(V) || {K,V} <- Reasons], ", "),
    exec:new(Action, Fixit, Node).
    
workflow(M, Obj) ->
    Key = get_key(Obj),
    
    Path = proplists:get_value(<<"script_path">>, Obj),
    Cmd = M:make_cmd(Path, "-a workflow"),
    Json = os:cmd(Cmd),

    logger:debug("~p:workflow(~p)",[M, Key]),
    {Raw, <<"0">>} = from_json(Json),

    % Populate all of the jobs
    _Kids = M:do_workflow(Raw, Obj), % FIXIT do we need result?
    
    Exec = exec:new(workflow, ok, erlang:node()),
    Obj1 = add_exec(Obj, Exec),
    _Obj2 = M:update(Obj1),

    logger:debug("~p:workflow(~p): done",[M,Key]),
    {ok,V} = M:select(Key),
    V.
    
run(M, Obj) ->
    Key = get_key(Obj),
    logger:debug("~p:run(~p)",[M,Key]),

    M:do_run(Obj),

    Exec = exec:new(run, ok, erlang:node()),
    Obj1 = add_exec(Obj, Exec),

    _Obj2 = M:update(Obj1),
    {ok,V} = M:select(Key),
    
    V.

finalize(M, Obj) ->
    Key = entity:get_key(Obj),

    Path = proplists:get_value(<<"script_path">>, Obj),
    Cmd = M:make_cmd(Path, "-a finalize"),
    logger:debug("~p:finalize(~p) Running ~p",[M, Key, Cmd]),    
    Final = os:cmd(Cmd),

    Raw = from_json(Final),

    Exec = script_result(finalize, Raw, erlang:node()),
    Obj2 = add_exec(Obj, Exec),

    _Obj3 = M:update(Obj2),
    logger:debug("~p:finalize(~p): done",[M,Key]),
    {ok,V} = M:select(Key),
    V.

complete(M, Obj) ->
    complete(M, Obj, ok, erlang:node()).
complete(M, Obj, Result, Node) ->
    add_state(M, Obj, complete, Result, Node).

cancel(M, Obj, Status) ->
    add_state(M, Obj, cancel, Status, erlang:node()).
    
errored(M, Obj, Reason) ->
    errored(M, Obj, Reason, erlang:node()).
errored(M, Obj, Reason, Node) ->
    add_state(M, Obj, error, Reason, Node).
    
add_state(M, Obj, Status, Result, Node) ->
    Obj2 = add_exec(Obj, exec:new(Status, Result, Node)),
    Obj3 = entity:update(M, Obj2),
    Key = entity:get_key(Obj3),
    {ok,V} = M:select(Key),
    V.
    
get_parent(Obj) ->
    get_link(get_parent_uri(Obj)).

get_link(undefined) -> undefined;
get_link(Url) ->
    [M,Id] = string:tokens(util:ensure_list(Url),"/"),
    (util:ensure_atom(M)):select(Id).
    
is_complete(Obj) ->
    latest_is(Obj, <<"complete">>).
is_finalized(Obj) ->
    latest_is(Obj, <<"finalized">>).
is_error(Obj) ->
    latest_is(Obj, <<"error">>).

can_cancel(Obj) ->
    (not latest_is(Obj, <<"finalize">>)) and
        (not latest_is(Obj, <<"cancel">>)).

latest_is(Obj, Status) ->
    proplists:get_value(<<"status">>, get_latest_exec(Obj)) =:= 
        util:ensure_binary(Status).
    
strip(Entity) ->
    lists:foldl(fun(K, E) -> lists:keydelete(K, 1, E) end,
                Entity, [<<"_id">>, <<"added">>, <<"uri">>]).

    
