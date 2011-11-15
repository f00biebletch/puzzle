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
% @doc Wrapper to pull puzzle content from repo.
%
% $Id$
%

-module(content_repo).

-behaviour(gen_server).

-export([start_link/0,terminate/2, stop/0]).
-export([init/1, handle_call/3, handle_cast/2,handle_info/2,
        code_change/3]).
-export([pull/1]).

-define(TOC, "config.spec").
-define(REPO_ROOT, "/mnt/input").
-define(INPUT_DEST, "input").
-define(SCOPE,{local, content_repo}).

init(_Arg) ->
    process_flag(trap_exit, true),
    logger:debug("~p:init(): starting",[?MODULE]),

    {ok, []}.

start_link() ->
    gen_server:start_link(?SCOPE, ?MODULE, [], []).

terminate(_Reason, _State) ->
    logger:info("~p:terminate()",[?MODULE]),
    ok.

handle_call({pull, Input}, _From, State) ->
    {reply, do_pull(Input), State};
handle_call(Msg, _From, State) ->
    {reply, Msg, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Msg, State) ->
    {Msg, normal, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
     {ok, State}.

pull(Input) ->
    gen_server:call(?MODULE, {pull, Input}).

stop() -> gen_server:cast(?MODULE, stop).

do_pull(Input) ->
    File = filename:join([Input,"input",?TOC]),
    logger:debug("~p:do_pull() File = ~p",
                 [?MODULE, File]),
    {ok, Dev} = file:open(File,[read]),
    {ok,Ignore} = re:compile("(^\\s*#|^\\s*$)"),
    Dest = filename:join(Input, ?INPUT_DEST),
    logger:debug("~p:do_pull() Dest = ~p",
                 [?MODULE, Dest]),
    read(Dest, none, Dev, Ignore),
    file:close(Dev),
    Dest.

read(Dest, SubDest, TocFile, Ignore) ->
    case io:get_line(TocFile,"") of
        eof -> ok;
        Line ->
            read(Dest, 
                 handle_line(Dest, SubDest, Line, Ignore), 
                 TocFile, 
                 Ignore)
    end.

handle_line(Dest, SubDest, Line, Ignore) ->
    case re:run(Line, Ignore) of
        {match,_} -> SubDest;
        _ ->
            % Just go ahead and copy the file now; no need to build
            % up a list and process later, but may want to think about
            % a pattern
            case string:tokens(string:strip(Line, right, $\n)," ") of
                [TargetDir] ->
                    Td2 = string:strip(TargetDir, left, $/),
                    Dest2 = filename:join([Dest, Td2]),
                    Md = "mkdir -p "++ Dest2,
                    Res = os:cmd(Md),
                    logger:debug("~p:handle_line() Cmd = ~p, Res = ~p",
                                 [?MODULE,Md,Res]),
                    Td2;
                [Folder,Version,File] ->
                    Src = filename:join([?REPO_ROOT, 
                                         string:strip(Folder,left,$/), 
                                         Version, 
                                         File]),
                    Target = filename:join([Dest, SubDest]),
                    Cp = "cp -R "++Src++" "++ Target,
                    Res = os:cmd(Cp),
                    logger:debug("~p:handle_line() Cmd = ~p, Res = ~p",
                                 [?MODULE,Cp,Res]),
                    SubDest
            end
    end.
            
            
            
            
            
    


