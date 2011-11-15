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
% @doc gen_server to monitor heartbeats.
%
% $Id$
%
-module(hbmonitor).

-behaviour(gen_server).
-export([start_link/0,terminate/2]).
-export([init/1, handle_call/3, handle_cast/2,handle_info/2,
        code_change/3]).

-include("pzd.hrl").

-define(SCOPE,{local, hbmonitor}).
-define(DEFAULT_DELTA, 3600).

-record(state, {delta}).

init(_Arg) ->
    process_flag(trap_exit, true),
    logger:debug("~p:init(): starting",[?MODULE]),

    timer:send_interval(interval_milliseconds(), interval),

    {ok, #state{delta=?DEFAULT_DELTA}}.

start_link() ->
    gen_server:start_link(?SCOPE, ?MODULE, [], []).

terminate(_Reason, _State) ->
    logger:info("~p:terminate()",[?MODULE]),
    ok.

handle_call({set_delta, Delta}, _From, State) ->
    {reply, ok, State#state{delta=Delta}};
handle_call(Msg, _From, State) ->
    {reply, Msg, State}.
handle_cast({post_heartbeat, HB}, State) ->
    gen_server:cast(cluster, {heartbeat, HB}),
    {noreply, State}.

handle_info(interval, State)->
    {noreply, sweep(State)};
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
     {ok, State}.
    
interval_milliseconds()-> 60000.

sweep(State) ->
    {ok, HBs} = heartbeat:all(),

    Downs = lists:filter(
              fun(HB) ->
                      Now = erlang:universaltime(),
                      Last = 
                          httpd_util:convert_request_date(
                            util:ensure_list(
                              proplists:get_value(<<"added">>, HB))),
                      (util:diff_seconds(Now, Last) > State#state.delta)
              end,
              HBs),
    _Actuals = gen_server:call(cluster, {missed_heartbeat, Downs}),
    % FIXIT need to store the actual downs and not keep whining about them
    State.
    
