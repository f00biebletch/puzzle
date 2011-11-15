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
% @doc Generic resource util.
%
% $Id$
%

-module(entity_resource).
-export([init/1,resource_exists/3,to_json/3, process_post/3]).
-export([delete_resource/3, is_authorized/3, generate_etag/3]).
-export([encodings_provided/3]).

-export([describe/1]).

-include("pzd.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {ok, #context{}}.
    
describe(M) -> 
    case M:entity() of
        none -> <<"">>;
        E -> util:ensure_binary(E:describe())
    end.
    
resource_exists(M, Req, Context) ->
    resource_exists(M, wrq:disp_path(Req), Req, Context).
resource_exists(M, "summary", Req, Context) ->
    {ok,All} = M:summary(wrq:req_qs(Req)),
    {true, Req, Context#context{key=all, value=All}};
resource_exists(M, [], Req, Context) ->
    {ok,All} = M:filter(wrq:req_qs(Req)),
    {true, Req, Context#context{key=all, value=All}};
resource_exists(M, Key, Req, Context) ->
    K = list_to_binary(Key),
    {ok,Obj} = M:select(K),
    case Obj of
        [] ->
            {false, Req, Context};
        [V|[]] ->
            {true, Req, Context#context{key=K, value=V}};
        V ->
            {true, Req, Context#context{key=K, value=V}}
    end.

encodings_provided(_M, Req, Context) ->  
    case wrq:method(Req) of
        'GET' ->
            {[{"identity", fun(X) -> X end},
              {"gzip", fun(X) -> zlib:gzip(X) end}], 
             Req, Context};
        _ ->
            {[{"identity", fun(X) -> X end}], Req, Context}
    end.

to_json(_M, Req, Context) ->
    {to_json(Context), Req, Context}.

to_json(Context) ->
    mochijson2:encode(entity:mochify(Context#context.value)).

generate_etag(_M, Req, Context) ->
    case entity:hash(Context#context.value) of
        undefined ->
            {undefined, Req, Context};
        Hash ->
            {util:ensure_list(Hash), Req, Context}
    end.

delete_resource(M, Req, Context) ->
    Key = wrq:disp_path(Req),
    logger:debug("~p:delete_resource(~p)",[M, Key]),
    _Result = M:delete(Key), % FIXIT do something with result
    {true, Req, Context}.

process_post(M, Req, Context) ->
    process_post(M, wrq:disp_path(Req), Req, Context).

process_post(M, [], Req, Context) ->
    Body = wrq:req_body(Req),
    logger:debug("~p:process_post(): Body = ~p",[M,Body]),

    {struct, V} = mochijson2:decode(Body),

    Path = util:ensure_list(proplists:get_value(<<"path">>, V)),

    % FIXIT need a way to trap errors and return http codes
    Obj = M:info(Path),
    Json = entity:to_json(Obj),

    Req1 = wrq:set_resp_header("Content-Type", "application/json",Req),
    Req2 = wrq:append_to_resp_body(Json, Req1),

    {true,Req2,Context#context{value=Obj}};
process_post(M, Key, Req, Context) ->
    Body = wrq:req_body(Req),
    logger:debug("~p:process_post(~p): Body = ~p",[M, Key, Body]),

    {struct, V} = mochijson2:decode(Body), 
    Action = proplists:get_value(<<"action">>, V),

    {ok, Obj} = M:select(Key),

    apply(M, util:ensure_atom(Action), [Obj]),
    
    {true, Req, Context}.

is_authorized(Module, Req, Context) ->
    case wrq:method(Req) of
        M when M == 'POST'; M == 'DELETE' -> 
            do_is_authorized(Module, Req, Context);
        _ -> 
            {true, Req, Context}
    end.

do_is_authorized(Module, Req, Context) ->
    case wrq:get_req_header("Authorization", Req) of
        "Puzzle "++Auth ->
            logger:debug("~p:is_authorized: ~p",[Module, Auth]),
            case handle_auth_header(Auth, Req) of
                {true, KeyUri} -> {true, Req, Context#context{user=KeyUri}};
                _ -> {"Basic realm=pzd", Req, Context}
            end;
        _ ->
            % FIXIT hack failover until I get JS auth working
            case handle_cookie(wrq:get_req_header("Cookie", Req)) of
                {true, KeyUri} -> {true, Req, Context#context{user=KeyUri}};
                _ ->
                    {"Basic realm=pzd", Req, Context}
            end
    end.

handle_auth_header(Auth, Req) ->
    [KeyId, Signature] = string:tokens(Auth,":"),
    Data = string_to_sign(Req),
    authorization:validate(KeyId, Signature, Data).

handle_cookie(Cookie) ->
    Raw = mochiweb_util:unquote(Cookie),
    [Key, Val] = 
        string:tokens(string:substr(Raw,string:str(Raw, "=")+1), "\$\$\$"),
    authorization:check_key(Key, Val).

string_to_sign(Req) ->
    M = atom_to_list(wrq:method(Req)),
    CType = wrq:get_req_header("Content-Type", Req),
    Date = wrq:get_req_header("Date", Req),
    Res = case wrq:disp_path(Req) of
              undefined -> "/";
              P -> "/"++P
          end,
    
    Str = M++"\n"++CType++"\n"++Date++"\n"++Res,                           
    logger:debug("string_to_sign() Str = ~p",[Str]),
    Str.

