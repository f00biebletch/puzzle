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
% @doc gen_server for web configuration.
%
% $Id$
%
-module(about).

-behaviour(gen_server).

-export([start_link/2,terminate/2]).
-export([init/1, handle_call/3, handle_cast/2,handle_info/2,
        code_change/3]).
-export([describe/0, hash/0]).

-include("pzd.hrl").

-define(SCOPE,{local, about}).

-record(state, {config, file}).

init([Config, File]) ->
    process_flag(trap_exit, true),
    logger:debug("~p:init(): starting",[?MODULE]),
    {ok, #state{config=Config, file=File}}.

start_link(Config, File) ->
    gen_server:start_link(?SCOPE, ?MODULE, [Config, File], []).

terminate(_Reason, _State) ->
    logger:info("~p:terminate()",[?MODULE]),
    ok.

handle_call({describe}, _From, State) ->
    {reply, to_meta(State#state.config), State};
handle_call({hash}, _From, State) ->
    {ok, Data} = file:read_file(State#state.file),
    {reply, util:sha(Data), State};
handle_call(Msg, _From, State) ->
    {reply, Msg, State}.

handle_cast(Msg, State) ->
    {Msg, normal, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
     {ok, State}.

describe() -> gen_server:call(?MODULE, {describe}).
hash() -> gen_server:call(?MODULE, {hash}).

to_meta(Config) ->
    lists:reverse(to_meta(proplists:get_value(dispatch, Config), [])).

to_meta([{Path, Module, _}|T], Paths) when is_list(Path) ->
    to_meta(T, [
                [{<<"path">>, util:ensure_binary(filename:join(Path))},
                 {<<"description">>, entity_resource:describe(Module)}]
            |
            Paths]);
to_meta([_|T], Paths) -> to_meta(T,Paths);
to_meta([], Paths) -> Paths.
    
