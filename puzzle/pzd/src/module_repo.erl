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
% @doc Wrapper to pull puzzle modules (suites) from revision control.
%
% $Id$
%

-module(module_repo).

-export([pull/2]).

-define(SVN_USER, "deviut").
-define(SVN_PASSWD, "deviut").

pull("svn://"++Path, Dest) ->
    Co = "svn --username "++ ?SVN_USER ++ " --password "++ ?SVN_PASSWD ++
        " --force export  "++Path++" "++Dest,
    Res = os:cmd(Co),
    logger:debug("~p:pull(svn) ~p -> ~p",[?MODULE,Co,Res]),
    Dest;
%% FIXIT TODO support for git to appeal to cool kiddies
pull("file://"++Path, Dest) -> % assume local!
    Cp = "cp -r "++Path++" "++Dest,
    Res1 = os:cmd(Cp),
    logger:debug("~p:pull(local) ~p -> ~p",[?MODULE,Cp,Res1]),
    Dest.

