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
% @doc Stats collection for reporting.
%
% $Id$
%

-module(misc).

-include_lib("kernel/include/file.hrl").
-export([collect/1]).
-record(tst_stat, {uri,count=0, file_size=0, file_count=0, cpu_time=0}).
-define(INPUT_ROOT, "input/obj_quality/").
-define(REPO_ROOT, "/mnt/input/video/raw/obj_quality").

collect(DateTime) ->
    collect([{"NAME","VDRtestbed"}, {"latest_exec.status","finalize"}],
           DateTime).

collect(Filter, DateTime) ->
    {ok, Tsts} = tst:filter(Filter),
    Flt = 
        lists:filter(fun(Tst) ->
                             Dt = calendar:datetime_to_gregorian_seconds(
                                    httpd_util:convert_request_date(
                                      util:ensure_list(
                                        proplists:get_value(<<"added">>, 
                                                            Tst)))),
                             Thresh = calendar:datetime_to_gregorian_seconds(
                                        DateTime),
                             (Dt - Thresh) > 0
                     end, Tsts),
                         
    [collect_tst(Tst) || Tst <- Flt].

collect_tst(Tst) ->
    Uri = entity:get_uri(Tst),
    {ok, Jobs} = job:filter([{"parent_uri", Uri}]),
    lists:foldl(fun(Job, Stat) ->
                        {ok, Tpts} = 
                            tpt:filter([{"parent_uri", entity:get_uri(Job)}]),
                        lists:foldl(fun count/2, Stat, Tpts)
                end,
                #tst_stat{uri=Uri}, Jobs).

count(Tpt, St=#tst_stat{count=Count,
                        file_size=FileSize,
                        file_count=FileCount,
                        cpu_time=CpuTime}) ->
    {Sz, FCt} = get_file_info(Tpt),
    CPU = get_cpu_time(Tpt),
    St#tst_stat{
      count=Count+1,
      file_size=FileSize+Sz,
      file_count=FileCount+FCt,
      cpu_time=CpuTime+CPU}.

    
get_exec(Tpt, St) ->
    State = util:ensure_binary(St),
    L = proplists:get_value(<<"latest_exec">>, Tpt),
    case (exec:get_status(L) =:= State) of
        true -> L;
        _ ->
            {array, Execs} = proplists:get_value(<<"execs">>, Tpt),
            case lists:dropwhile(fun(E) ->
                                    exec:get_status(E) =/= State
                            end, Execs) of
                [] -> [];
                V -> hd(V)
            end
    end.

get_file_info(Tpt) ->
    lists:foldl(fun(F, {Sz, Count}) ->
                        Size = get_file_size(resolve_file(Tpt, F)),
                        {Size+Sz, Count+1}
                end,
                {0, 0}, tpt:get_input_files(Tpt)).

resolve_file(Tpt, F) ->
    case string:str(F, ?INPUT_ROOT) of
        0 ->
            filename:join(tpt:get_fs_root(Tpt), F);
        X ->
            Fn = string:substr(F, length(?INPUT_ROOT)+X),
            filename:join(get_repo_root(Fn), Fn)
    end.
    
get_cpu_time(Tpt) ->
    Start = 
        httpd_util:convert_request_date(
          util:ensure_list(exec:get_timestamp(get_exec(Tpt, started)))),
    End = 
        httpd_util:convert_request_date(
          util:ensure_list(exec:get_timestamp(get_exec(Tpt, complete)))),

    calendar:datetime_to_gregorian_seconds(End) -
        calendar:datetime_to_gregorian_seconds(Start).
    

get_file_size(F) ->
    case file:read_file_info(F) of
        {ok, Info} -> 
            file:read_file_info(F),
            Info#file_info.size;
        Other -> 
            io:format("error opening ~p: ~p~n",[F, Other]),
            0
    end.

get_repo_root(Fn) ->
    [_Name, Res|_] = string:tokens(Fn, "_"),
    filename:join([?REPO_ROOT,lookup(Res)]).

lookup("176x144") -> "qcif/000000000012";
lookup("320x240") -> "qvga/000000000011";
lookup("352x288") -> "cif/000000000016";
lookup("480x320") -> "hvga/000000000017";
lookup("640x480") -> "vga/000000000018";
lookup("720x480") -> "ntsc-sd/000000000019";
lookup("720x576") -> "pal-sd/000000000020";
lookup("1280x720") -> "hd/000000000013";
lookup("1920x1080") -> "fhd/000000000022".

    
    
    
    
                             
                             
