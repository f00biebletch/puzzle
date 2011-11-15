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
% @doc Unit tests for nodeinfo
%
% $Id$
%
-module(nodeinfo_tests).

-include_lib("eunit/include/eunit.hrl").
% FIXIT this should be scoped by test for job!
test_data() ->
    [
     {<<"HW">>,
      [{<<"ISET">>,<<"SSE2">>},
       {<<"CHECK">>,<<"ge">>},
       {<<"ARCH">>,<<"x86">>},
       {<<"CPU">>,
        [
         {<<"NAME">>,<<"xeon">>},
         {<<"BRAND">>,<<"intel">>},
         {<<"MODEL">>,3040},
         {<<"CODENAME">>,<<"prestonia">>}
        ]
       }
      ]
     },
     {<<"OS">>,
      [{<<"BIT">>,32},
       {<<"BRAND">>,<<"any">>},
       {<<"CHECK">>,<<"eq">>},
       {<<"VERSION">>,
        [
         {<<"MINOR">>,<<"any">>},
         {<<"MAJOR">>,<<"any">>}
        ]
       },
       {<<"TYPE">>,<<"linux">>}]
     }
    ].

info_test_() ->
    Info = nodeinfo:get_info(),
    ?_assert(nodeinfo:maybe_run(test_data(), Info)).
