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
% @doc A test suite.
%
% $Id$
%

-module(tst).

-include("pzd.hrl").

-export([pre_commit/1, post_commit/1,sanitize/1]).
-export([save/1,all/0,select/2,select/1,delete/1,filter/1,filter/2,update/1]).
-export([info/1,new/1,workflow/1,run/1,do_workflow/2,do_run/1]).
-export([maybe_complete/1,complete/1,finalize/1,cancel/1]).
-export([make_cmd/2, clean/1]).
-export([get_log_file/1, for_rfs/3]).
-export([summary/1, describe/0]).

-define(OUTPUT_BASE, "/mnt/output/pzmodules").

summary(P) -> entity:summary(?MODULE, P).
    
describe() -> "Test suite that is composed of jobs".
    
sanitize(T) -> 
    Fld = fun(K) -> 
                  proplists:is_defined(K,T)
          end,
    case lists:all(Fld, [<<"NAME">>,
                         <<"script_path">>,
                         <<"INPUT">>,
                         <<"OUTPUT">>])
        of
        true ->
            T;
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

% can't run tst from some other path, need to cd for each command;
% this is not true of job/tpt
make_cmd(Path, Args) ->
    P2 = filename:join([util:ensure_list(Path), ?TST_LOC_CODE, ?TST_BOOT_SCRIPT]),
    "cd "++filename:dirname(P2)++"; ./"++filename:basename(P2)++" "++Args.

info(Path) -> entity:info(?MODULE, Path).
new(Tst) -> entity:new(?MODULE, Tst).
workflow(Tst) -> entity:workflow(?MODULE, Tst).
run(Tst) -> entity:run(?MODULE, Tst).
complete(Tst) -> entity:complete(?MODULE, Tst).
finalize(Tst) -> entity:finalize(?MODULE, Tst).
cancel(Tst) -> 
    case entity:can_cancel(Tst) of
        true ->
            Res = tstomatic:cancel_tst(Tst),
            entity:cancel(?MODULE, Tst, Res);
        false ->
            Tst
    end.

maybe_complete(Tst) ->
    {ok, Jobs} = job:filter([{"parent_uri", entity:get_uri(Tst)}]),
    case lists:all(fun(T) -> entity:is_complete(T) end, Jobs) of
        true ->
            Tst2 = complete(Tst),
            {true, Tst2};
        _ -> 
            case lists:any(fun(J) -> entity:is_error(J) end, Jobs) of
                true -> {error, Tst};
                _ -> {false, Tst}
            end            
    end.

clean(Tst) ->
    Path = proplists:get_value(<<"PATH">>, 
                               proplists:get_value(<<"INPUT">>,Tst)),
    Cmd = "rm -rf "++filename:join([util:ensure_list(Path),"*"]),
    Res = os:cmd(Cmd),
    logger:debug("~p:clean(~p) ~p -> ~p",[?MODULE,entity:get_key(Tst),Cmd,Res]).
    
do_workflow(Json, Tst) ->
    TstUri = entity:get_uri(Tst),

    Proc = fun(Module) ->
                   RawJob = proplists:get_value(Module, Json),

                   ScriptName = 
                       util:ensure_list(
                         proplists:get_value(<<"FILE">>, RawJob)),
                   Path = job:build_path(Module, ScriptName, Tst),
                   job:info(Path, TstUri)
           end,
    lists:map(Proc, proplists:get_keys(Json)).

do_run(_Tst) -> ok.

for_rfs(Rfs, Release, Pzm) ->
    PzmPath = util:ensure_list(pzm:repo_path(Pzm)),
    PzmName = util:ensure_list(pzm:name(Pzm)),

    Path = bootstrap(Release, PzmPath, PzmName),

    Tst = tst:info(Path),
    Tst2 = lists:append(Tst,
                        [{<<"rfs_uri">>, entity:get_uri(Rfs)},
                         {<<"pzm_uri">>, entity:get_uri(Pzm)},
                         {<<"release_uri">>, entity:get_uri(Release)}]),
    tst:update(Tst2),
    {ok, Tst3} = tst:select(entity:get_key(Tst2)),
    Tst3.

bootstrap(Release, PzmPath, ModuleName) ->
    Root = init_fs(ModuleName),
    _Dest = module_repo:pull(PzmPath, Root),
    resolved = release:resolve(Release, Root),

    {ok, ContentRepo} = content_repo:start_link(),    
    Content = gen_server:call(ContentRepo, {pull, Root}, infinity),
    content_repo:stop(),

    link_content(Root, Content),
    list_to_binary(Root).

init_fs(Module) ->
    Timestamp = 
        lists:flatten(
          io_lib:format("~4..0B~2..0B~2..0B-~2..0B~2..0B~2..0B",
                        lists:flatten([ tuple_to_list(X) || 
                                          X <- tuple_to_list(
                                                 erlang:localtime())]))),
    Root = filename:join([?OUTPUT_BASE,Module,Timestamp]),
    Mkdir = "mkdir -p "++Root,
    Res = os:cmd(Mkdir),
    logger:debug("~p:init_fs(): ~p -> ~p",[?MODULE,Mkdir,Res]),
    Root.

link_content(_Root, _Content) ->
    do_something_fixit.
     
% FIXIT move to entity?    
get_log_file(Tst) ->
    Root = job:get_fs_root(Tst),

    TstName = util:ensure_list(
                proplists:get_value(<<"NAME">>, Tst)),
    filename:join([
                   Root, 
                   "output", % FIXIT use Tst.OUTPUT?
                   TstName++".log"
                  ]).

