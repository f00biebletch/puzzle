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
% @doc Application header.
%
% $Id$
%

%% FIXIT wtf is this?  Does is need to be here?
-record(context, {key, value, pre_commit, post_commit, user}).

% FIXIT is this nested or linked?  I like linked to allow audit
-record(exec, {status, result, timestamp, node}).

-define(PZD_USER, "kevinmcintire").

-define(HW_DOMAIN,"core.net").
-define(LOCAL_DOMAIN,"grid.net").

-define(TST_LOC_CODE, "tstCode").
-define(TST_LOC_CONFIGS, "configs").
-define(TST_LOC_BIN, "bin").
-define(TST_LOC_VIEW, "tstView").

-define(TST_BOOT_SCRIPT, "tstAPI.pl").

-define(TPT_TMP_ROOT, "/mnt/output/tmp").
