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
% @doc Callbacks for the pzd application.
%
% $Id$
%

-module(pzd).

-behaviour(application).
-export([start/2,stop/1,reload/1,version/0]).

-include("pzd.hrl").

%% @spec start(_Type, StartArgs) -> ServerRet
%% @doc application start callback for pzd.
start(_Type, _StartArgs) ->
    mongodb:singleServer(def),
    mongodb:connect(def),
    pzd_deps:ensure(),
    error_logger:add_report_handler(notification),
    Root = pzd_sup:start_link(),
    logger:announce("~p:start(): pzd booting",
                    [?MODULE]),
    Root.

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for pzd.
stop(_State) ->
    logger:info("~p:terminate()",[?MODULE]),
    ok.

version() ->
   lists:keyfind(pzd, 1, application:which_applications()).
 
reload(_Tok) ->   
    lists:foreach(fun(M) ->
                          logger:debug("~p:reload(~p)",[?MODULE, M]),
                          code:purge(M),
                          code:load_file(M)
                  end,
                  [ M || {M, P} <- code:all_loaded(), 
                         string:str(util:ensure_list(P), "pzd-0.7.0") > 0]).

