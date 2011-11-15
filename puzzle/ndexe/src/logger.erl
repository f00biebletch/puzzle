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
% @doc Simple logger.  We don't need crazy log4* jang since erlang
% has good runtime support for redirects.
%
% $Id$
%
-module(logger).
-compile({no_auto_import,[error/2]}).
-export([debug/1,info/1,warn/1,error/1]).
-export([debug/2,info/2,warn/2,error/2]).

debug(Msg) -> debug(Msg, []).
info(Msg) -> info(Msg, []).
warn(Msg) -> warn(Msg, []).
error(Msg) -> error(Msg, []).
debug(Msg, Args) -> log(debug, Msg, Args).
info(Msg, Args) -> log(info, Msg, Args).
warn(Msg, Args) -> log(warn, Msg, Args).
error(Msg, Args) -> log(error, Msg, Args).

log(Level, Msg, Args) ->
    io:format(string:join(
                [
                 httpd_util:rfc1123_date(erlang:localtime()),
                 make_level(Level),
                 get_node(),
                 Msg
                ], " ")++"\n", Args).

make_level(debug) -> "[DEBUG]";
make_level(info) -> "[INFO]";
make_level(warn) -> "[WARN]";
make_level(error) -> "[ERROR]".
    
get_node() -> "("++atom_to_list(node())++")".
