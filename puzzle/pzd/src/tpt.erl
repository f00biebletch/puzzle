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
% @doc A test point.
%
% $Id$
%

-module(tpt).

-include("pzd.hrl").

-export([pre_commit/1, post_commit/1,sanitize/1]).
-export([save/1,all/0,select/2,select/1,delete/1,filter/1,filter/2,update/1]).
-export([info/2,new/1,workflow/1,run/1,complete/3]).
-export([make_cmd/2,do_run/1]).
-export([get_fs_root/1, stage/3,build_path/2]).
-export([clean/1, errored/3]).
-export([get_log_file/1]).
-export([get_input_files/1]).
-export([summary/1, describe/0]).
-export([unique_root_names/1]).

-define(MAX_XFER_FILES, 20).

summary(P) -> entity:summary(?MODULE, P).

describe() -> "A single test or execution against specified input and configuration".

sanitize(Tpt) -> 
    Fld = fun(K) -> 
                  proplists:get_value(K,Tpt) =:= undefined
          end,
    case lists:all(Fld, [<<"name">>,<<"os">>,<<"arch">>]) of
        true ->
            Tpt;
        _ ->
            invalid
    end.

pre_commit(Tpt) -> entity:pre_commit(Tpt).
post_commit(Tpt) -> Tpt.

all() -> entity:all(?MODULE).
delete(K) -> entity:delete(?MODULE, K).
filter(P) -> entity:filter(?MODULE, P).
filter(P, S) -> entity:filter(?MODULE, P, S).
save(Obj) -> entity:save(?MODULE, Obj).
update(Obj) -> entity:update(?MODULE, Obj).
select(K) -> entity:select(?MODULE, K).
select(K, Decode) -> entity:select(?MODULE, K, Decode).

make_cmd(Path, Args) -> entity:make_cmd(Path, Args).

info(Path, ParentId) -> entity:info(?MODULE, Path, ParentId).
new(Tpt) -> entity:new(?MODULE, Tpt).
workflow(Tpt) -> Tpt.
run(Tpt) -> entity:run(?MODULE, Tpt).
complete(Tpt, Result, Node) -> entity:complete(?MODULE, Tpt, Result, Node).

errored(Tpt, Reason, Node) -> entity:errored(?MODULE, Tpt, Reason, Node).

do_run(Tpt) -> Tpt.

get_fs_root(Obj) ->
    Path = filename:dirname(
             util:ensure_list(proplists:get_value(<<"script_path">>, Obj))),
    Rel = util:ensure_list(proplists:get_value(<<"TSTRELPATH">>, Obj)),
    P2 = filename:join(Path, Rel),
    string:strip(os:cmd("cd "++P2++"; pwd"), right, $\n).    

get_input_files(Tpt) ->
    Input = proplists:get_value(<<"INPUT">>, Tpt),
    Dir = util:ensure_list(proplists:get_value(<<"DIR">>, Input, "./")),
    Data = proplists:get_value(<<"DATA">>, Input),

    Files = do_get_input_files(Dir, Data),
    lists:map(fun({file, F}) ->
                      F
              end, lists:flatten(Files)).

do_get_input_files(_Dir, []) -> [];
do_get_input_files(_Dir, <<>>) -> [];
do_get_input_files(Dir, Props) ->
    lists:map(fun(Key) ->
                      Entry = proplists:get_value(Key, Props),
                      get_input_file(Dir, Entry)
              end,
              proplists:get_keys(Props)).

get_input_file(MainDir, Props) ->
    Dir = util:ensure_list(
            proplists:get_value(<<"DIR">>, Props)),
    case Dir of
        "undefined" -> do_get_input_files(MainDir, Props);
        _ ->
            File = util:ensure_list(
                     proplists:get_value(<<"FILE">>, Props)),
            {file, filename:join(["input",MainDir,Dir,File])}
    end.

do_links(Tpt, Path, StageFile) ->
    % make input dir on input folder
    Input = filename:join([transpose_path(Path), "input"]),
    InputLink = filename:join([Path,"input"]),
    do_link(Tpt, Input, InputLink, StageFile),

    Output = filename:join([Path, "output"]),
    OutputLink = filename:join([transpose_path(Path),"output"]),
    do_link(Tpt, Output, OutputLink, StageFile).

do_link(Tpt, DirName, LinkName, StageFile) ->
    Cmd = "if [ ! -d "++ DirName ++" ]; then "++
        " mkdir -p "++DirName++" && "++
        "ln -s "++DirName++" "++LinkName++
        "; fi~n",
    logger:debug("~p:do_link(~p): Cmd = ~p",
                 [?MODULE, entity:get_key(Tpt), Cmd]),
    io:fwrite(StageFile, Cmd, []).
    
pre_stage(Tpt, Host, Path) ->
    Key = entity:get_key(Tpt),
    TmpRoot = ?TPT_TMP_ROOT,
    StageName = filename:join([TmpRoot,
                               util:ensure_list(Key)++".sh"]),
    {ok, FD} = file:open(StageName, [write]),
    logger:debug("Creating file ~p", [StageName]),
    io:fwrite(FD, "mkdir -p "++Path++"~n", []),

    do_links(Tpt, Path, FD),
    pre_inputs(Tpt, Path, FD),
    pre_outputs(Tpt, FD),

    file:close(FD),
    Dest = StageName,

    Work = [
            "ssh "++Host++" 'mkdir -p "++TmpRoot++"'",
            util:make_scp(StageName, Host, Dest),
            "ssh "++Host++" 'chmod +x "++Dest++" && "++Dest++
            " && rm -f "++Dest++"'",
            "rm -f "++StageName
           ],
    lists:foreach(fun(Cmd) ->
                          Res = os:cmd(Cmd),
                          logger:debug("~p:pre_stage(~p): ~p -> ~p",
                                       [?MODULE, Key, 
                                        Cmd, Res])
                  end, Work).    
    
pre_inputs(Tpt, Path, StageFile) ->
    Inputs = find_input_files(Path, Tpt),

    Dirs = sets:to_list(sets:from_list([filename:dirname(X) || X <- Inputs])),
    lists:foreach(fun(Dir) ->
                          Cmd = "mkdir -p "++Dir++"~n",
                          logger:debug("~p:pre_inputs(~p): ~p",
                                       [?MODULE, entity:get_key(Tpt), Cmd]),
                          io:fwrite(StageFile, Cmd, [])
                  end, Dirs).
    
pre_outputs(Tpt, StageFile) ->
    [_,Output] = result:make_result_dir(Tpt),

    Cmd = "mkdir -p "++Output++"~n",
    io:fwrite(StageFile, Cmd, []).
    
stage(Tpt, Node, Path) ->

    [_, Name] = node:parse_name(Node),

    Host = ?PZD_USER++"@"++Name,

    logger:debug("~p:stage(~p) to ~p",[?MODULE, entity:get_key(Tpt), Name]),

    pre_stage(Tpt, Host, Path),

    stage_code(Tpt, Host, Path),

    stage_context(Tpt, Node),

    stage_outputs(Tpt, Host),
    
    stage_inputs(Tpt, Path, Host),
        
    ok.

stage_outputs(Tpt, Host) ->
    [_,Output] = result:make_result_dir(Tpt),

    Lcmd = util:make_scp(filename:join([Output,"*.log"]), Host, Output),
    LOcmd = os:cmd(Lcmd),
    logger:debug("~p:stage_outputs(~p): ~p -> ~p",
                 [?MODULE, entity:get_key(Tpt), Lcmd, LOcmd]),
    true = validate_copy(LOcmd).

validate_copy([]) -> true;
validate_copy(Str) -> 
    Allowed = ["permission denied", "text file busy"],
    S = string:to_lower(Str),
    Bools = lists:map(fun(V) -> string:str(S, V) end,
                      Allowed),
    case lists:any(fun(Idx) -> Idx > 0 end, Bools) of
        true -> true;
        _ -> Str
    end.

transpose_path(Path) ->
    [A,B,C|Rest] = filename:split(Path),
    do_transpose(A,B,C,Rest).
do_transpose("/","mnt","output", Path) ->
    filename:join("/mnt/input",filename:join(Path));
do_transpose("/","mnt","input", Path) ->
    filename:join("/mnt/output",filename:join(Path)).

copy_files(Tpt, Host, Srcs, Dests, Validate) ->
    Copier = fun({Src, Dest}) ->
                     Cmd = util:make_scp(Src,Host,Dest),
                     Res = os:cmd(Cmd),
                     logger:debug(
                       "~p:copy_files(~p): ~p ==> ~p",
                       [?MODULE, entity:get_key(Tpt), Cmd, Res]),
                     case Validate of
                         true ->
                             true = validate_copy(Res);
                         _ ->
                             ok
                     end
             end,
    lists:foreach(Copier, lists:zip(Srcs, Dests)).
    
find_input_files(Root, Tpt) ->
    case get_input_files(Tpt) of
        [] -> [];
        Inputs -> [filename:join([Root, IF]) || IF <- Inputs ]
    end.
    
stage_inputs(Tpt, Path, Host) ->
    
    Inputs = find_input_files(Path, Tpt),

    Txts = lists:foldl(fun(F, Acc) ->
                               case raw_to_txt(F) of
                                   {none} -> Acc;
                                   {file, Txt} -> [Txt|Acc]
                               end
                       end,
                       [], Inputs),

    transfer_files(Tpt, Host, Inputs, Txts),

    ok.

stage_code(Tpt, Host, Path) ->
    % FIXIT make this more efficient? yes use tranfer_files
    P2 = filename:join([Path, ?TST_LOC_CODE, ?TST_BOOT_SCRIPT]),
    Cmd = "ssh "++Host++" 'if [ ! -f "++ P2 ++" ]; then echo dork; fi'",
    Res = string:strip(os:cmd(Cmd), right, $\n),
    logger:debug("~p:stage_code(~p): ~p ==> ~p",
                 [?MODULE, entity:get_key(Tpt), Cmd, Res]),
    do_stage_code(Tpt, Host, Path, Res =:= "dork").

do_stage_code(Tpt, Host, Path, true) ->
    copy_files(Tpt, Host,  
               [
                filename:join([Path, ?TST_LOC_CODE]),
                filename:join([Path, ?TST_LOC_CONFIGS]),
                filename:join([Path, ?TST_LOC_BIN]),
                filename:join([Path, ?TST_LOC_VIEW])
               ],
               [Path, Path, Path, Path], true
              );
do_stage_code(_Tpt, _Host, _Path, _) ->
    ok.
    

local_transfer(Root, Srcs) ->
    Local = lists:map(fun(P) ->
                              ["/"|Pth] = filename:split(P),
                              filename:join([Root, filename:join(Pth)])
                      end,
                      Srcs),
    lists:foreach(fun({Src,Dst}) ->
                          os:cmd("mkdir -p "++filename:dirname(Dst)),
                          os:cmd("cp "++util:ensure_list(Src)++" "++
                                 util:ensure_list(Dst))
                  end,
                  lists:zip(Srcs, Local)).
    
transfer_files(Tpt, Host, Inputs, Txts) when length(Inputs) > ?MAX_XFER_FILES ->
    TmpRoot = filename:join([?TPT_TMP_ROOT,
                             util:ensure_list(entity:get_key(Tpt))]),
    [] = os:cmd("mkdir "++TmpRoot),
    logger:debug("~p:tranfer_files(~p, ~p): root = ~p",
                 [?MODULE, entity:get_key(Tpt), Host, TmpRoot]),

    local_transfer(TmpRoot, Inputs),
    local_transfer(TmpRoot, Txts),

    Cmd = util:make_scp(filename:join([TmpRoot, "*"]), Host, "/"),
    logger:debug("~p:transfer_files(~p, ~p): Cmd = ~p",
                 [?MODULE, entity:get_key(Tpt), Host, Cmd]),
    validate_copy(os:cmd(Cmd)),
    os:cmd("rm -rf "++TmpRoot);
transfer_files(Tpt, Host, Inputs, Txts) ->
    copy_files(Tpt, Host, Inputs, Inputs, true),
    copy_files(Tpt, Host, Txts, Txts, false).    
    
build_path(ScriptName, Job) ->
    Base = 
        filename:dirname(util:ensure_list(
                           proplists:get_value(<<"script_path">>, 
                                               Job))),
    Base ++"/tpt/" ++ ScriptName.

clean(none) -> 
    logger:debug("~p:clean: nothing to clean",
                 [?MODULE]);

clean(RootName) ->

    TupleRootName = lists:nth(1,RootName),
    Root = element(1,TupleRootName),

    Name = element(2,TupleRootName),

    do_clean(Root, Name).

do_clean(Root, Name) ->
    % ASSUME: identical file structure on controller/node
    % so that script "-a new" is simple.
            
    Cmd = "ssh "++?PZD_USER++"@"++Name++
        " 'rm -rf "++Root++" && "++     % FIXIT this does not work locally
        " rm -rf "++transpose_path(Root)++"'",
    Res = os:cmd(Cmd),
    logger:debug("~p:do_clean(): ~p ==> ~p",
                 [?MODULE, Cmd, Res]),
    Root.

get_node_of_execution(Tpt) ->
    % FIXIT for now only remove if complete - eventually want to ask node
    % that it was scheduled for
    Exec = entity:get_latest_exec(Tpt),
    case exec:get_status(Exec) of
        <<"complete">> ->
            exec:get_node(Exec);
        _ -> none
    end.

stage_context(Tpt, Node) ->
    logger:debug("~p:stage_context(~p): enter",
                 [?MODULE, entity:get_key(Tpt)]),
    Iset = util:ensure_binary(
             string:join([util:ensure_list(V) || 
                             V <- proplists:get_value(
                                    <<"instr_set">>, Node)], ",")),

    logger:debug("~p:stage_context(~p): Iset = ~p",
                 [?MODULE, entity:get_key(Tpt), Iset]),
    Hw = proplists:get_value(<<"HW">>, Node),
    Instr = {<<"ISET">>, Iset},
    Hw2 = case proplists:is_defined(<<"ISET">>, Hw) of
              true -> lists:keyreplace(<<"ISET">>, 1, Hw, Instr);
              _ -> [Instr|Hw]
          end,
    logger:debug("~p:stage_context(~p): Hw2 = ~p",
                 [?MODULE, entity:get_key(Tpt), Hw2]),
              
    Node2 = lists:keyreplace(<<"HW">>, 1, Node, {<<"HW">>, Hw2}),
    Node3 = lists:foldl(
              fun(Key, Acc) -> 
                      [{Key, proplists:get_value(Key, Node2)} | Acc]
              end, 
              [], [<<"HOST">>, <<"HW">>, <<"OS">>]),

    Node4 = [{<<"tpt_uri">>, entity:get_uri(Tpt)}|Node3],
    Json = entity:to_json(Node4),
    LogName = get_hostinfo_file(Tpt),
    logger:debug("~p:stage_context(~p): ~p",
                 [?MODULE, entity:get_key(Tpt), LogName]),
    {ok, FD} = file:open(LogName, [write]),
    io:fwrite(FD, Json, []),
    file:close(FD).
    
get_log_file(Tpt) ->
    get_log_file(Tpt, ".log").
get_hostinfo_file(Tpt) ->
    get_log_file(Tpt, ".hostinfo.log").

get_log_file(Tpt, Ext) ->
    {ok, Job} = entity:get_parent(Tpt),

    Root = tpt:get_fs_root(Tpt),

    JobDir = util:ensure_list(
               proplists:get_value(<<"DIR">>,
                                   proplists:get_value(<<"OUTPUT">>,Job))),
    TptDir = util:ensure_list(
               proplists:get_value(<<"DIR">>,
                                   proplists:get_value(<<"OUTPUT">>,Tpt))),
    TptName = util:ensure_list(
               proplists:get_value(<<"NAME">>, Tpt)),
    filename:join([
                   Root, 
                   "output", % FIXIT use Tst.OUTPUT?
                   JobDir,
                   TptDir,
                   TptName++Ext
                  ]).
         
raw_to_txt(RawFile) ->
    case string:str(RawFile,".raw") of
        0 -> {none};
        Idx -> {file, string:substr(RawFile,1,Idx-1)++".txt"}
    end.

root_names([Tpt|Tpts],[]) ->
    Node = get_node_of_execution(Tpt),
    Root = tpt:get_fs_root(Tpt),
    [_,Name] = string:tokens(util:ensure_list(Node),"@"),
    RootName = [{Root,Name}],

    root_names(Tpts,[RootName]);

root_names([Tpt|Tpts],RootNames) ->
    Node = get_node_of_execution(Tpt),
    Root = tpt:get_fs_root(Tpt),
    [_,Name] = string:tokens(util:ensure_list(Node),"@"),
    RootName = [{Root,Name}],

    root_names(Tpts,[RootName|RootNames]);

root_names([],RootNames) ->
    RootNames.

unique_root_names(Tpts) ->
 
    RootNames = root_names(Tpts,[]),
    SetRootNames = sets:from_list(RootNames),
    
    sets:to_list(SetRootNames).
