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
% @doc gen_server for email, unix style.
%
% $Id$
%
-module(mailer).

-behaviour(gen_server).

-export([start_link/0,terminate/2]).
-export([init/1, handle_call/3, handle_cast/2,handle_info/2,
        code_change/3]).
-export([add_to/1, remove_to/1, flush/0, get_tos/0,send/2, purge/0]).

-include("pzd.hrl").

-define(SCOPE,{local, mailer}).
-define(DEFAULT_SUBJECT, "Puzzle Notification").
-define(DEFAULT_TO, ["whomever@wherever.com"]).
-define(SEPERATOR, "\n\n======================================\n\n").
-define(SMTP_RELAY, {smtp_host,"mail-relay"}).

-record(state, {to, msgs}).

init(_Arg) ->
    process_flag(trap_exit, true),
    timer:send_interval(interval_milliseconds(), interval),
    logger:debug("~p:init(): starting",[?MODULE]),
    {ok, #state{to=?DEFAULT_TO, msgs=dict:new()}}.

start_link() ->
    gen_server:start_link(?SCOPE, ?MODULE, [], []).

terminate(_Reason, State) ->
    Msg = io_lib:format("~p:terminate()",[?MODULE]),
    logger:info(Msg), % hack don't call announce unless u wanna deadlock
    sweep(State#state{msgs=dict:append(info, Msg, State#state.msgs)}),
    ok.

handle_call({add_to, To}, _From, State) ->
    {reply, To, State#state{to=[To|State#state.to]}};
handle_call({remove_to, To}, _From, State) ->
    {reply, To, State#state{to=lists:delete(To,State#state.to)}};
handle_call({get_tos}, _From, State) ->
    {reply, State#state.to, State};
handle_call(Msg, _From, State) ->
    {reply, Msg, State}.
handle_cast({flush}, State) ->
    {noreply, sweep(State)};
handle_cast({purge}, State) ->
    {noreply, State#state{msgs=dict:new()}};
handle_cast({send, Level, Data}, State) ->
    {noreply, State#state{msgs=dict:append(Level, Data, State#state.msgs)}};
handle_cast(Msg, State) ->
    {Msg, normal, State}.

handle_info(interval, State)->
    {noreply, sweep(State)};
handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
     {ok, State}.

add_to(To) ->
    gen_server:call(?MODULE, {add_to, To}).
remove_to(To) ->
    gen_server:call(?MODULE, {remove_to, To}).
flush() ->
    gen_server:cast(?MODULE, {flush}).
get_tos() ->
    gen_server:call(?MODULE, {get_tos}).
send(Level, Data) ->
    gen_server:cast(?MODULE, {send, Level, Data}).
purge() ->
    gen_server:cast(?MODULE, {purge}).

interval_milliseconds()-> 10000.

sweep(State) ->
    Msgs = dict:to_list(State#state.msgs),
    process(Msgs, State),
    State#state{msgs=dict:new()}.

process([], _State) -> ok;
process([{Level, Data}|Rest], State) ->
    do_send(from(), tos(State#state.to), subject(Level), body(Data)),
    process(Rest, State).

do_send(From, To, Subject, Body) ->
    smtp_client:deliver(From, 
                        To,
                        Subject, 
                        Body, 
                        [?SMTP_RELAY]).

from() -> util:ensure_list(node()).
tos(Tos) -> string:join(Tos, ",").
subject(Level) -> ?DEFAULT_SUBJECT ++ " [" ++ 
                      string:to_upper(util:ensure_list(Level))++"]".
body(Data) -> string:join(Data, ?SEPERATOR).
    
    
