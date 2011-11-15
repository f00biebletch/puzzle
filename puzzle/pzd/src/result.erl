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
% @doc The result (output) of a tpt.
%
% $Id$
%
-module(result).

-include("pzd.hrl").

-export([collect/2, get_root/1, make_result_dir/1]).

% FIXIT this may need to be persisted as an entity!

collect(NodeName, Tpt) ->
    logger:debug("~p:collect(~p)",
                 [?MODULE, entity:get_key(Tpt)]),
    [Src,Dest] = make_result_dir(Tpt),
    
    Mcmd = "mkdir -p "++Dest,
    _Mkdir = os:cmd(Mcmd),

    Cmd = util:make_scp()++" "++?PZD_USER++"@"++NodeName++":"++Src++" "++Dest,
    Res = os:cmd(Cmd),
    logger:debug("~p:collect(~p): ~p -> ~p",
                 [?MODULE, entity:get_key(Tpt), Cmd, Res]),
    Dest.

get_root(Job) ->
    {ok,Tst} = entity:get_parent(Job),
    filename:join([
                   "/",
                   "mnt",
                   "output",
                   util:ensure_list(entity:get_key(Tst))
                   ]).
    
make_result_dir(Tpt) ->    
    {ok,Job} = entity:get_parent(Tpt),
    % FIXIT need a new function
    Root = tpt:get_fs_root(Tpt),

    % yeah Job.OUTPUT.DIR
    JobDir = util:ensure_list(
               proplists:get_value(<<"DIR">>,
                                   proplists:get_value(<<"OUTPUT">>,Job))),
    TptDir = util:ensure_list(
               proplists:get_value(<<"DIR">>,
                                   proplists:get_value(<<"OUTPUT">>,Tpt))),
    Dest = filename:join([
                          Root, 
                          "output", % FIXIT use Tst.OUTPUT?
                          JobDir,
                          TptDir
                         ]),
    Src = filename:join([Dest, "*"]),
    [Src, Dest].

