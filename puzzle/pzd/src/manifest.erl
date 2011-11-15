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
% @doc A build manifest.
%
% $Id$
%

-module(manifest).
-include("pzd.hrl").

-export([fetch/2, parse/1]).

-define(MANIFEST_DIR, "/tmp").

fetch({Server, User, Passwd}, Path) ->
    logger:debug("~p:fetch(~p)",[?MODULE, Path]),
    %% MP = util:ensure_list(Path),
    %% {ok, Pid} = inets:start(ftpc, [{host, Server}]),
    %% ftp:user(Pid, User, Passwd),
    %% Local = stick_it_here(MP),
    %% logger:debug("~p:fetch(~p) Local = ~p",[?MODULE, Path, Local]),
    %% ftp:recv(Pid, MP, Local),
    %% inets:stop(ftpc, Pid),
    %% Local.
    "FIXIT".

parse(File) ->
    {ok, Data} = file:read_file(File),
    {struct, V} = mochijson2:decode(Data),
    logger:debug("~p:parse(): V = ~p",[?MODULE, V]),                          
    fixit.
    

stick_it_here(File) ->
    filename:join([?MANIFEST_DIR, filename:basename(File)]).
