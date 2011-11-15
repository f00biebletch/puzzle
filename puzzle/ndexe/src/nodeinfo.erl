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
% @doc Gather node information
%
% $Id$
%
-module(nodeinfo).

-export([get_info/0, is_cygwin/1]).

-define(DATA_INPUT, <<"/mnt/input/node">>).
-define(DATA_OUTPUT, <<"/mnt/output/node">>).

os_type() ->
    case filelib:is_file("/proc/cpuinfo") of
        true ->
            linux;
        _ ->
            case filelib:is_file("/mach_kernel") of
                true ->
                    osx;
                _ ->
                    unknown
            end
    end.

get_info() ->
    Info = get_info(os_type()),
    maybe_unscrew_cygwin(Info).

get_info(linux) ->
    Hostname = string:strip(os:cmd("hostname"), right, $\n),
    Uname = os:cmd("uname -rsm"),
    [Os, OsVer, Hardware] = string:tokens(Uname, " "),
    Machine = string:strip(Hardware, right, $\n),
    [_, V] = string:tokens(
               os:cmd("cat /proc/cpuinfo |grep flags|head -1"),":"),
    Flags = [list_to_binary(
               string:strip(string:strip(F, right, $\n), right, $\r)) 
             || F <-["scalar"|string:tokens(V," ")]],
    [_, Cpu1] = string:tokens(
                 os:cmd("cat /proc/cpuinfo |grep \"model name\"|head -1"),":"),
    Cpu = string:join(
            string:tokens(string:strip(Cpu1, right, $\n)," ")," "),
    CpuCount = 
        list_to_integer(
          string:strip(
            os:cmd("cat /proc/cpuinfo |grep processor|wc -l"), right, $\n)),
    {ndexe, _, Version} = 
        lists:keyfind(ndexe, 1, application:loaded_applications()),

    [
     {<<"version">>, list_to_binary(Version)},
     {<<"name">>, list_to_binary(atom_to_list(erlang:node()))},
     {<<"hostname">>,list_to_binary(Hostname)},
     {<<"arch">>,<<"x86">>},
     {<<"os">>, list_to_binary(Os)},
     {<<"os_version">>, list_to_binary(OsVer)},
     {<<"machine">>, list_to_binary(Machine)},
     {<<"instr_set">>, Flags},
     {<<"cpu_info">>, list_to_binary(Cpu)},
     {<<"cpu_count">>, CpuCount},
     {<<"input_path">>, ?DATA_INPUT},
     {<<"output_path">>, ?DATA_OUTPUT}
    ];
get_info(osx) ->
    Hostname = string:strip(os:cmd("hostname"), right, $\n),
    Uname = os:cmd("uname -rsm"),
    [Os, OsVer, Hardware] = string:tokens(Uname, " "),
    Machine = string:strip(Hardware, right, $\n),

    V = os:cmd("sysctl -n machdep.cpu.features"),
    Flags = [list_to_binary(
               string:to_lower(string:strip(string:strip(F, right, $\n), right, $\r)))
             || F <-["scalar"|string:tokens(V," ")]],
    Cpu1 = os:cmd("sysctl -n machdep.cpu.features"),
    Cpu = string:strip(Cpu1, right, $\n),
    [_,Count1] = string:tokens(os:cmd("sysctl hw.ncpu"), ":"),
    CpuCount = 
        list_to_integer(
          string:strip(string:strip(Count1), right, $\n)),
    {ndexe, _, Version} = 
        lists:keyfind(ndexe, 1, application:loaded_applications()),

    [
     {<<"version">>, list_to_binary(Version)},
     {<<"name">>, list_to_binary(atom_to_list(erlang:node()))},
     {<<"hostname">>,list_to_binary(Hostname)},
     {<<"arch">>,<<"x86">>},
     {<<"os">>, list_to_binary(Os)},
     {<<"os_version">>, list_to_binary(OsVer)},
     {<<"machine">>, list_to_binary(Machine)},
     {<<"instr_set">>, Flags},
     {<<"cpu_info">>, list_to_binary(Cpu)},
     {<<"cpu_count">>, CpuCount},
     {<<"input_path">>, ?DATA_INPUT},
     {<<"output_path">>, ?DATA_OUTPUT}
    ].
    
maybe_unscrew_cygwin(Info) ->
    CygOs = proplists:get_value(<<"os">>,Info),
    case string:str(string:to_lower(
                      binary_to_list(CygOs)), "cygwin") of
        0 -> Info;
        _ -> do_unscrew([{<<"cygwin">>,CygOs}|Info])
    end.
    
do_unscrew(Info) ->
    [OsDirty, VerDirty] = string:tokens(os:cmd("cmd /c ver"),"["),
    Os1 = string:strip(OsDirty, both, $\r),
    Os2 = string:strip(Os1, both, $\n),
    Os = string:strip(Os2),
    Info1 = lists:keyreplace(<<"os">>, 1, Info, {<<"os">>, list_to_binary(Os)}),
    Ver1 = string:strip(VerDirty, right, $\n),
    Ver2 = string:strip(Ver1, right, $\r),
    [_,OsVersion] = string:tokens(string:strip(Ver2, right, $])," "),
    lists:keyreplace(<<"os_version">>, 1, Info1, 
                     {<<"os_version">>, list_to_binary(OsVersion)}).

is_cygwin(Info) ->    
    proplists:is_defined(<<"cygwin">>, Info).
    
    
