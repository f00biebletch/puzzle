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
% @doc A exec.
%
% $Id$
%

-module(exec).

-include("pzd.hrl").

-export([new/3]).
-export([get_status/1, get_node/1, get_result/1, get_timestamp/1]).

new(Status, Result, Node) ->
    [{<<"status">>, util:ensure_binary(Status)}, 
     {<<"result">>,util:ensure_binary(Result)}, 
     {<<"node">>, util:ensure_binary(Node)}, 
     {<<"timestamp">>,
      util:ensure_binary(httpd_util:rfc1123_date(erlang:localtime()))}].

get_status(Exec) -> proplists:get_value(<<"status">>, Exec).
get_node(Exec) -> proplists:get_value(<<"node">>, Exec).
get_result(Exec) -> proplists:get_value(<<"result">>, Exec).
get_timestamp(Exec) -> proplists:get_value(<<"timestamp">>, Exec).
    
