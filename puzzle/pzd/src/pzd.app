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
{application, pzd,
 [{description, "Puzzle"},
  {vsn, "0.7.0"},
  {modules, [
    about,
    about_resource,
    access_key,
    access_key_resource,
    authorization,
    cluster,
    crypto_util,
    entity,
    exec,
    heartbeat,
    heartbeat_resource,
    hbmonitor,
    job,
    job_exec,
    job_resource,
    pzm,
    pzm_resource,
    pzd,
    pzd_deps,
    entity_resource,
    pzd_sup,
    logger,
    mailer,
    manifest,
    ndsched,
    node,
    node_resource,
    notification,
    release,
    release_resource,
    result,
    rfs,
    rfs_resource,
    smtp_client,
    static_resource,
    tpt,
    tpt_exec,
    tpt_resource,
    tst,
    tstomatic,
    tst_exec,
    tst_resource,
    util,
    content_repo,
    build_repo,
    misc,
    module_repo,
    node_proxy
  ]},
  {registered, []},
  {mod, {pzd, []}},
  {sasl, [{sasl_error_logger, {file, "priv/log/sasl.log"}},
          {error_logger_mf_dir, "priv/log/sasl"},
          {error_logger_mf_maxfiles, 20},
          {error_logger_mf_maxbytes, 524288},
          {releases_dir, "releases"}
         ]},
  {env, []},
  {applications, [kernel, stdlib, crypto, xmerl, inets, erlmongo]}]}.
