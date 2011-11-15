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
% @doc error/info notification event handler
%
% $Id$
%
-module(notification).

-behaviour(gen_event).

-include("pzd.hrl").

-export([init/1, handle_event/2, handle_call/2, handle_info/2,
        terminate/2, code_change/3]).

-export([publish/2]).

init([]) ->
    {ok, []}.
handle_event({error, _Lead, {_Pid, Format, Data}}, State) ->
    publish(error, io_lib:format(Format, Data)),
    {ok, State};
handle_event({error_report, _Lead, {_Pid, std_error, Data}}, State) ->
    publish(error, io_lib:format("~p~n", [Data])),
    {ok, State};
handle_event({error_report, _Lead, {_Pid, _Type, Report}}, State) ->
    publish(error, io_lib:format("~p~n", [Report])),
    {ok, State};
handle_event({warning_msg, _Lead, {_Pid, Format, Data}}, State) ->
    publish(warning, io_lib:format(Format, Data)),
    {ok, State};
handle_event({warning_report, _Lead, {_Pid, std_warning, Data}}, State) ->
    publish(warning, io_lib:format("~p~n", [Data])),
    {ok, State};
handle_event({warning_report, _Lead, {_Pid, _Type, Report}}, State) ->
    publish(warning, io_lib:format("~p~n", [Report])),
    {ok, State};
handle_event({info_msg, _Lead, {_Pid, Format, Data}}, State) ->
    logger:debug("~p:info_msg(): Format = ~p, Data = ~p",
                 [?MODULE, Format, Data]),
    {ok, State};
handle_event({info_report, _Lead, {_Pid, std_info, Data}}, State) ->
    logger:debug("~p:info_msg(): Data = ~p",
                 [?MODULE, Data]),
    {ok, State};
handle_event({info_report, _Lead, {_Pid, _Type, Report}}, State) ->
    logger:debug("~p:info_msg(): Report = ~p",
                 [?MODULE, Report]),
    {ok, State};
handle_event(_Event, State) ->
    {ok, State}.
handle_call(Msg, State) ->
    {ok, Msg, State}.
handle_info(_Info, State) ->
    {ok, State}.
terminate(_Arg, _State) ->
    ok.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

publish(Level, Data) ->
    gen_server:cast(mailer, {send, Level, Data}).

