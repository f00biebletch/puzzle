//
// Copyright 2011 Kevin McIntire, Gianluca Filippini
//
// Licensed under the Apache License, Version 2.0 (the "License"); you may not 
// use this file except in compliance with the License. You may obtain a copy 
// of the License at 
//
//    http://www.apache.org/licenses/LICENSE-2.0 
//
// Unless required by applicable law or agreed to in writing, software 
// distributed under the License is distributed on an "AS IS" BASIS, 
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
// See the License for the specific language governing permissions and 
// limitations under the License. 
//
function Controller()
{
    this.show = function(uri, $container) {
        var view = this.makeView(this);
        $.getJSON(uri, function(data){
                      view.show(data, $container);
                  });
    };

    this.fetch = function(uri) {
        var obj = null;
        $.ajax({
                   url: uri,
                   async: false,
                   dataType: 'json',
                   success: function (data) {
                       obj = data;
                   }
               });
        return obj;
    };

    this.makeView = function(controller) { return null; };
    this.makeUri = function(parentUri) { return null; };
};

function TstController() {

    this.makeUri = function() {
        return '/tst/summary';
    };
    
    this.makeView = function(controller) {
        return new TstView(controller);
    };

    this.list = function($container) {
        var view = this.makeView(this);
        $.getJSON(this.makeUri(), function(data){
                      view.list(data, $container);
                  });
    };

    this.show = function(uri, $container) {
        var view = this.makeView(this);
        $.getJSON(uri, function(tst){
                      tst.jobs = app.job.fetchGrouped(tst.uri);
                      tst.rfs = app.rfs.fetch(tst.rfs_uri);
                      tst.pzm = app.pzm.fetch(tst.pzm_uri);
                      view.show(tst, $container);
                  });
    };

};

TstController.prototype = new Controller();

function RfsController() {

    this.makeUri = function(parentUri) {
        return '/rfs';        
    };

    this.fetch = function(uri) {
        var rfs = null;
        $.ajax({
                   url: uri,
                   async: false,
                   dataType: 'json',
                   success: function (data) {
                       rfs = data;
                       rfs.release = app.release.fetch(rfs.release_uri);
                       rfs.accessKey = app.accessKey.fetch(rfs.user);
                   }
               });
        return rfs;
    };
};
RfsController.prototype = new Controller();

function PzmController() {

    this.makeUri = function(parentUri) {
        return '/pzm';        
    };    
};
PzmController.prototype = new Controller();

function ReleaseController() {

    this.makeUri = function(parentUri) {
        return '/release';        
    };    
};
ReleaseController.prototype = new Controller();

function AccessKeyController() {

    this.makeUri = function(parentUri) {
        return '/access_key';        
    };    
};
AccessKeyController.prototype = new Controller();

function JobController(controller) {
    this.controller = controller;

    this.makeUri = function(parentUri) {
        return '/job/summary?parent_uri='+parentUri;
    };

    this.makeView = function(controller) {
        return new JobView(controller);
    };

    this.getBucketStatus = function(jobs) {
        var stati = jQuery.map(jobs, function(job, idx) {
                                   if (job.latest_exec)
                                       return job.latest_exec.status;
                                   return null;
                               });
        // oh my this is embarrasing
        if ($.inArray("error",stati) != -1) return "error";
        if ($.inArray("info",stati) != -1) return "info";
        if ($.inArray("workflow",stati) != -1) return "workflow";
        if ($.inArray("run",stati) != -1) return "run";
        if ($.inArray("complete",stati) != -1) return "complete";
        return "unknown";
    };

    this.fetchGrouped = function(parentUri) {
        var uri = this.makeUri(parentUri);
        var grouped = {};
        $.ajax({
                   url: uri,
                   async: false,
                   dataType: 'json',
                   success: function (jobs) {
                       $.each(jobs,function(index, job) { 
                                  var priority = job.PRIORITY;
                                  var arr = grouped[priority] || [];
                                  arr.push(job);
                                  grouped[priority] = arr;
                              });
                   }
               });
        // sort and return as [{priorty,[jobs]}]
        var keys = [];
        for (var key in grouped) {
            keys.push(parseInt(key));
        }
        keys.sort();
        var sorted = [];
        $.each(keys, function(idx, k) {
                   var j = grouped[k];
                   sorted.push({priority:k, jobs: j});
               });
        return sorted;
    };

    this.show = function(uri, $container) {
        $('#busy').show();
        var view = this.makeView(this);
        $.getJSON(uri, function(job){
                      job.tpts = app.tpt.fetchForJob(job.uri);
                      view.show(job, $container);
                  });
    };
};
JobController.prototype = new Controller();

function TptController() {

    this.makeUri = function(parentUri) {
        return '/tpt/summary?parent_uri='+parentUri;
    };

    this.makeView = function(controller) {
        return new TptView(controller);
    };

    this.fetchForJob = function(parentUri) {
        var uri = this.makeUri(parentUri);
        var tpts = null;
        $.ajax({
                   url: uri,
                   async: false,
                   dataType: 'json',
                   success: function (data) {
                       tpts = data;
                   }
               });
        return tpts;
    };
};
TptController.prototype = new Controller();

function ClusterController() {

    this.update = function() {
        var view = new ClusterView();
        $.getJSON('/node/status', function(data){
                      view.update(data);
                  });
    };

    this.show = function() {
        var view = new ClusterView();
        $.getJSON('/node', function(data){                      
                      view.show(data);
                  });
    };

    this.monitor = function() {
    };

    this.stopMonitor = function() {
        var view = new ClusterView();
        view.timeout();
    };
};

function AuthController() {

    this.signReq = function(xhr){
        var x = this.sign();
        if (x == null){
            throw "Error: set key";
        }
        var res = xhr.setRequestHeader("Authorization", escape(x.auth));
        res = xhr.setRequestHeader("Content-type", x.type);
        res = xhr.setRequestHeader("Date", x.date);
        
    };

    this.sign = function() {
        var token = this.getToken();
        if (token == null)
            return null;
        var theTime = Util.getRFC822Date(new Date());
        var ctype = "application/json";
        var toSign = "POST\n"+ctype+"\n"+theTime+"\n/";
        var keyData = $().crypt({method:"b64dec",source:token.key});
        var hmac = Crypto.HMAC(Crypto.SHA1, toSign, keyData);;
        var sig = $().crypt({method:"b64enc",source:hmac}); 
        var auth = "Puzzle "+token.keyId+":"+sig;
        return {
            date: theTime, type: ctype, authorization: auth
        };
    };

    this.storeToken = function(keyId, key, days) {
        var data = keyId+"$$$"+key;
        $.cookie(this.getName(), data, { path: '/', expires: days });
    };

    this.isLoggedIn = function() {
        return (this.getToken() != null) && (this.getToken().key != null);
    };

    this.getToken = function() {
        var cookie = $.cookie(this.getName());
        if (cookie != null) {
            var vals = cookie.split("$$$");
            return {
                keyId: vals[0],
                key: vals[1]
            };
        }
        else return null;
        
    };

    this.getName = function() {
        return "pzAuth-"+location.hostname;
    };
};

