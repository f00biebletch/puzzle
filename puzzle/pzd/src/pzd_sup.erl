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
% @doc Supervisor for the pzd application.
%
% $Id$
%

-module(pzd_sup).

-behaviour(supervisor).

-export([start_link/0, upgrade/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
	    [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
		      supervisor:terminate_child(?MODULE, Id),
		      supervisor:delete_child(?MODULE, Id),
		      ok
	      end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

init([]) ->
    ConfigFile = filename:join(
                   [
                    filename:dirname(code:which(?MODULE)),
                    "..", 
                    "priv", 
                    "www.conf"
                   ]),
    {ok, WebConfig} = file:consult(ConfigFile),
    {ok, {{one_for_one, 10, 10}, 
          [
           {webmachine_mochiweb,
            {webmachine_mochiweb, start, [WebConfig]},
            permanent, 
            5000, 
            worker, 
            dynamic},
           {mailer_main,
            {mailer, start_link, []},
            permanent,
            10000,
            worker,
            [mailer]},
           {hb_monitor_master,
            {hbmonitor, start_link, []},
            permanent,
            10000,
            worker,
            [hbmonitor]},
           {tst_o_matic,
            {tstomatic, start_link, []},
            permanent,
            10000,
            worker,
            [tstomatic]},
           {about,
            {about, start_link, [WebConfig, ConfigFile]},
            permanent,
            10000,
            worker,
            [about]},
           {cluster_main,
            {cluster, start_link, []},
            permanent,
            10000,
            worker,
            [cluster]}
          ]
         }}.

