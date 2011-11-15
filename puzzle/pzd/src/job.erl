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
% @doc A job in the pzmodules sense of the term; a set of
%  test points; a tst has 1 or more jobs.
%
% $Id$
%

-module(job).

-include("pzd.hrl").

-export([pre_commit/1, post_commit/1,sanitize/1]).
-export([save/1,all/0,select/2,select/1,delete/1,filter/1,filter/2,update/1]).
-export([info/2,new/1,workflow/1,run/1,complete/1,finalize/1]).
-export([do_workflow/2, make_cmd/2,do_run/1]).
-export([build_path/3]).
-export([errored/2, errored/3]).
-export([maybe_complete/1, clean/1]).
-export([get_log_file/1]).
-export([get_fs_root/1]).
-export([summary/1, describe/0]).

summary(Param) ->
    entity:summary(?MODULE, 
                   Param, 
                   [{"NAME",1}, 
                    {"TITLE",1},
                    {"DETAIL",1},
                    {"PRIORITY",1},
                    {"uri",1},
                    {"latest_exec.status",1}, 
                    {"latest_exec.timestamp",1}]).

describe() -> "A job is a logical container of test points".

sanitize(J) -> J.
pre_commit(J) -> entity:pre_commit(J).
post_commit(J) -> J.

all() -> entity:all(?MODULE).
delete(K) -> entity:delete(?MODULE, K).
filter(P) -> entity:filter(?MODULE, P).
filter(P, S) -> entity:filter(?MODULE, P, S).
save(Obj) -> entity:save(?MODULE, Obj).
update(Obj) -> entity:update(?MODULE, Obj).
select(K) -> entity:select(?MODULE, K).
select(K, Decode) -> entity:select(?MODULE, K, Decode).

make_cmd(Path, Args) -> entity:make_cmd(Path, Args).

info(Path, ParentId) -> entity:info(?MODULE, Path, ParentId).
new(Job) -> entity:new(?MODULE, Job).
workflow(Job) -> entity:workflow(?MODULE, Job).
run(Job) -> entity:run(?MODULE, Job).
complete(Job) -> entity:complete(?MODULE, Job).
finalize(Job) -> entity:finalize(?MODULE, Job).

maybe_complete(Job) ->
    {ok, Tpts} = tpt:filter([{"parent_uri", entity:get_uri(Job)}]),
    case lists:all(fun(T) -> entity:is_complete(T) end, Tpts) of
        true ->
            J2 = complete(Job),
            {true, J2};
        _ -> 
            case lists:all(fun(T) -> 
                                   entity:is_error(T) or 
                                       entity:is_complete(T) end, 
                           Tpts) of
                true -> {error, Job};
                _ -> {false, Job}
            end
    end.
    
do_workflow(Raw, Job) ->
    JobUri = entity:get_uri(Job),

    Proc = fun(Key) ->
                   Meta = proplists:get_value(Key, Raw),

                   Script = 
                       util:ensure_list(proplists:get_value(<<"FILE">>, Meta)),
                   Path = tpt:build_path(Script, Job),
                   tpt:info(Path, JobUri)
           end,
    lists:map(Proc, proplists:get_keys(Raw)).

do_run(Job) -> Job.

errored(Job, Reason) -> entity:errored(?MODULE, Job, Reason).
errored(Job, Reason, Node) -> entity:errored(?MODULE, Job, Reason, Node).

build_path(JobName, ScriptName, Tst) ->
    filename:join([
                   util:ensure_list(
                     proplists:get_value(<<"TSTRELPATH">>, Tst)),
                   "job_" ++ util:ensure_list(JobName),
                   ScriptName
                  ]).

clean(Job) ->
    {ok, Tpts} = tpt:filter([{"parent_uri", entity:get_uri(Job)}]),
    % FIXIT we need to group by priority and then run/schedule
    
    RootNames = tpt:unique_root_names(Tpts),
    
    % FIXIT TODO need to clean up if running > 1 machine
    lists:foreach(fun(RootName) -> tpt:clean(none) end, RootNames),
    
    cleaned.
    
% FIXIT move to entity?    
get_log_file(Job) ->
    Root = job:get_fs_root(Job),

    % yeah Job.OUTPUT.DIR
    JobDir = util:ensure_list(
               proplists:get_value(<<"DIR">>,
                                   proplists:get_value(<<"OUTPUT">>,Job))),
    JobName = util:ensure_list(
               proplists:get_value(<<"NAME">>, Job)),
    filename:join([
                   Root, 
                   "output", % FIXIT use Tst.OUTPUT?
                   JobDir,
                   JobName++".log"
                  ]).

get_fs_root(Obj) ->
    Path = filename:dirname(
             util:ensure_list(proplists:get_value(<<"script_path">>, Obj))),
    Rel = util:ensure_list(proplists:get_value(<<"TSTRELPATH">>, Obj)),
    P2 = filename:join([Path, Rel, ".."]),
    string:strip(os:cmd("cd "++P2++"; pwd"), right, $\n).    

