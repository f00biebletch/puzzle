%
% Copyright 2011 Kevin McIntire, Gianluca Filippini
% All Rights Reserved
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
% @doc gen_server for ndexe.
%
% $Id$
%
-module(ndexe_app).

-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for ndexe.
start(_Type, StartArgs) ->
    executor_sup:start_link(StartArgs).

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for ndexe.
stop(_State) ->
    ok.

