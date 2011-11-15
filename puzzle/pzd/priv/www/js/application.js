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
function Puzzle() {
    
    var tst = null;
    var job = null;
    var tpt = null;
    var cluster = null;
    var rfs = null;
    var pzm = null;
    var release = null;
    var accessKey = null;
    var auth = null;

    this.init = function() {

        this.tst = new TstController();
        this.job = new JobController();
        this.tpt = new TptController();
        this.cluster = new ClusterController();
        this.rfs = new RfsController();
        this.pzm = new PzmController();
        this.release = new ReleaseController();
        this.accessKey = new AccessKeyController();
        this.auth = new AuthController();

        $('#tabs').tabs();

        // FIXIT apply/bind
        var pz = this;
        $('#tabs .Suite').click(function() {
                                    pz.tst.list(pz.getCenter());
                                    stopMonitor();
                                    return true;
                                });
        $('#tabs .Cluster').click(function() {
                                      pz.cluster.show();
                                      monitorCluster(pz);
                                      return true;
                                });
        pz.tst.list(pz.getCenter());

    	$("#setAuth").dialog({
			         bgiframe: true,
			         autoOpen: false,
			         modal: true,
			         buttons: 
                                 {
				    'Set Key': function() {
                                        var keyId = $('#authKeyId').val();
                                        var key = $('#authKeyData').val();
                                        pz.auth.storeToken(keyId, key, 60);
					$(this).dialog('close');
				    }
				},
				Cancel: function() {
				    $(this).dialog('close');
				}
			    });
    
        $('#set-auth').click(function() {
			        $('#setAuth').dialog('open');
		            });

    	$("#runRfs").dialog({
			        bgiframe: true,
			        autoOpen: false,
			        modal: true,
			        buttons: 
                                {
				    'Run RFS': function() {
                                        var rfsName = $('#rfsName').val();
                                        var pzmPath = $('#rfsPzmPath').val();
                                        var json = JSON.stringify(
                                            {
                                                release: 
                                                {
                                                    manifest:
                                                    "manifest.txt",
                                                    // FIXIT wtf is this?
                                                    version: "1.2.3"
                                                },
                                                pzm: 
                                                [{
                                                     name:rfsName,
                                                     setup: "*",
                                                     pzm_path: pzmPath
                                                 }]
                                            });
                                        $.ajax({
                                                   type: 'POST',
                                                   url: '/rfs',
                                                   data: json,
                                                   timeout: 5000,
                                                   async: true,
                                                   processData: false,
                                                   beforeSend: function(xhr) {
                                                       try {
                                                           app.auth.signReq(xhr);
                                                       } catch (x) {
                                                           alert(x);
                                                           return false;
                                                       }
                                                       return true;
                                                   },
                                                   complete:  function() {
                                                       app.tst.list(pz.getCenter());
                                                   }
                                               });
                                        app.tst.list(pz.getCenter());
					$(this).dialog('close');
				    }
				},
				Cancel: function() {
				    $(this).dialog('close');
				}
			    });
    
        $('#run-rfs').click(function() {
			        $('#runRfs').dialog('open');
		            });

        $("#createNode").dialog({
			        bgiframe: true,
			        autoOpen: false,
			        height: 400,
			        width: 600,
			        modal: true,
			        buttons: 
                                {
				    'Create a node': function() {
                                        var nodeName = $('#name').val();
                                        var hostName = $('#host').val();
                                        var cpuName = $('#cpuName').val();
                                        var codeName = $('#cpuCodeName').val();
                                        var n = nodeName+'@'+hostName;
                                        $.post('/node', 
                                               JSON.stringify(
                                                   {name:n, 
                                                    cpu_name: cpuName, 
                                                    cpu_codename: codeName}),
                                               function(data, stat){
                                                   app.cluster.show();
                                               },
                                               "json");
					$(this).dialog('close');
				    }
				},
				Cancel: function() {
				    $(this).dialog('close');
				}
			    });
    
        $('#create-node').click(function() {
			            $('#createNode').dialog('open');
		                });
        
        $('#image-nodes').click(function() {
                                    $.post('/node/all', 
                                           JSON.stringify({action: "image"}),
                                           function(data, stat){
                                               app.cluster.show();
                                           },
                                           "json");
		                });

        $('#upgrade-nodes').click(function() {
                                      $.post('/node/all', 
                                             JSON.stringify(
                                                 {action: "upgrade"}),
                                             function(data, stat){
                                                 app.cluster.show();
                                             },
                                             "json");
		                });

        if (this.auth.isLoggedIn()) {
            $('#set-auth').hide();
        }
        else
        {
            $('#run-rfs').hide();
            $('#upgrade-nodes').hide();
            $('#image-nodes').hide();
            $('#create-node').hide();
        }

        $('#cancel-tst').hide();

        $('#upgrade-nodes').confirm();
        $('#image-nodes').confirm();
    	
    };

    this.getCenter = function() {
        return $('#tst .container');
    };

};

function Node(data) {
    this.data = data;

    this.getStatus = function() {
        var stat = this.data.status;
        if (stat == 'down') return "down";
        var running = this.data.running_tpts;
        if (running && running.length > 0) return "running";
        var staging = this.data.staging;
        if (staging != undefined && staging != "undefined") return "staging";
        return "idle";
    };    

    this.render = function() {
        var $el = $('<div id="node-'+this.data._id+
                        '"></div>');
        $el.addClass('ui-corner-all'); 
        $el.addClass('ui-widget-content');
        $el.addClass('node');

        var $content = 
            $('<div style="font-size: 0.8em"></div>').html(this.data.HOST.NAME);
        $el.append($content);

        $el.addClass("status-"+this.getStatus());
        $el.append($content);

        var $details = this.makeDetails();
        $el.append($details);
        
        this.makeDialog($details);

        $el.click(function(){
                      $details.dialog('open');
                      });
        return $el;

    };

    this.makeDialog = function($tgt) {
        var node = this.data;
        var buttons = this.makeButtons();
        $tgt.dialog({autoOpen:false, 
                       title: node.name,
                       show: 'slide',
                       width: 600,
                       closeOnEscape: true,
                       modal: true,
		       buttons: buttons
                      });

    };

    this.makeButtons = function() {
        var node = this.data;
        var buttons = {};

        if (app.auth.isLoggedIn()) {
            buttons["Start"] = function() {
                $.post('/node/'+node._id,
                       JSON.stringify({action: "start"}),
                       function(data, stat){
                           app.cluster.update();
                       },
                       "json");
                $(this).dialog('close');
            };
            buttons["Stop"] = function() {
                $.post('/node/'+node._id,
                       JSON.stringify({action: "stop"}),
                       function(data, stat){
                           app.cluster.update();
                       },
                       "json");
                $(this).dialog('close');
            };
            buttons["Upgrade"] = function() {
                $.post('/node/'+node._id,
                       JSON.stringify({action: "upgrade"}),
                       function(data, stat){
                           app.cluster.update();
                       },
                       "json");
                $(this).dialog('close');
            };
        }
        return buttons;
    };

    this.update = function() {
        var $node = $('#node-'+this.data._id);
        if (!$node)
        { 
            return; 
        }

        var newStat = this.getStatus();
        var clz = $node.attr('class');
        if (clz == null) return;
        var classes = clz.split(' ');
        if (classes == null || classes.length == 0) 
        {
            return;
        }
        $.each(classes, function(idx, cls) {
                   if (cls.indexOf("status-") === 0) {
                       $node.removeClass(cls);
                   }
               });
        $node.addClass("status-"+newStat);

        $node.find(".stagingList").replaceWith(this.makeStaging());
        $node.find(".runningList").replaceWith(this.makeRunning());

        var $details = $node.find('.details');
    };

    this.makeDetails = function() {
        var $details = $('<div id="details-'+this.data._id+'"></div>');
        $details.addClass('details');
        var $el = $('<div></div>');
        $el.addClass('proplist');
        //append here
        var $list = $('<ul></ul>');
        var node = this.data;
        $list.append(Util.makeListItem('Name',node.name));
        $list.append(Util.makeListItem('Host',node.HOST.NAME));
        $list.append(Util.makeListItem('CPU Name',node.HW.CPU.NAME));
        $list.append(Util.makeListItem('CPU Model',node.HW.CPU.MODEL));
        $list.append(Util.makeListItem('CPU Codename',node.HW.CPU.CODENAME));
        $list.append(Util.makeListItem('CPU Count',
                                          node.cpu_count));
        $list.append(Util.makeListItem('CPU Info',
                                          node.cpu_info));
        $list.append(Util.makeListItem('OS',
                                          node.OS.BRAND+" "+
                                          node.OS.TYPE+" [ "+
                                          node.HW.ARCH+" "+
                                          node.OS.BIT+" ]"));
        $list.append(Util.makeListItem('Puzzle Version', node.version));

        var $stCon = $('<div class="stagingList"></div>');
        $stCon.append(this.makeStaging());
        $list.append(Util.makeListItem('Staging',
                                       $stCon));
        var $runningCon = $('<div class="runningList"></div>');
        $runningCon.append(this.makeRunning());
        $list.append(Util.makeListItem('Running',
                                       $runningCon));

        $el.append($list);

        var $nodeControl = $('<div></div>');
        $details.append($el);
        $details.hide();
        return $details;
    };

    this.makeRunning = function() {
        var run = this.data.running_tpts;
        var id = this.data._id;
        if (run && run.length > 0) {
            var $runLinks = $('<ul></ul>');
            $.each(run, function(index, uri) {
                       var $li = $('<li></li>');
                       // FIXIT why are we doing xhr from render??
                       var $lnk = $('<a href="">'+uri+'</a>');
                       $lnk.click(function () {
                                      $('#details-'+id).dialog('close');
                                      app.tpt.show(uri, app.getCenter());
                                  });
                       $li.append($lnk);
                       $runLinks.append($li);
                   });
            return $runLinks;
        }
        else return $('<span>None</span>');

    };

    this.makeStaging = function() {
        var staging = this.data.staging;
        if (staging != undefined && staging != "undefined") {
            // FIXIT why are we doing xhr from render??
            var $lnk = $('<a href="#">'+staging+'</a>');
            $lnk.click(function () {
                           $('#details-'+id).dialog('close');
                           app.tpt.show(staging, app.getCenter());
                       });
            return $lnk;
        }
        else
        {
            return $('<span>None</span>');
        }
    };

};

function Timeline(obj) {

    this.data = obj.execs;
    if (this.data != null)
        this.data.unshift(obj.latest_exec);
    else
        this.data = [obj.latest_exec];

    this.render = function() {
        var exec = this.data;
        var $content = $('<div></div>');
        $content.addClass('content');

        var $table = $('<table width="100%"></table>');
        var $thead = $('<thead></thead>');
        var $header = $('<tr>');
        $header.append('<th>Status</th>');
        $header.append('<th>Result</th>');
        $header.append('<th>Time</th>');
        $header.append('<th>Node</th>');
        $thead.append($header);
        var $tbody = $('<tbody></tbody>');
        $.each(this.data, function(index, exec) {
                   $row = $('<tr>');
                   $row.append($('<td>').append(exec.status));
                   $row.append($('<td>').append(exec.result));
                   $row.append($('<td>').append(Util.formatDate(exec.timestamp)));
                   $row.append($('<td>').append(exec.node));
                   $tbody.append($row);
               });
        $table.append($thead);
        $table.append($tbody);

        $content.append($table);
                
        return $content;
    };

    this.getStatus = function() {
        if (this.data.latest_exec)
            return this.data.latest_exec.status;
        return null;
    };
};

function Tst(data) {
    this.data = data;
    
    this.canCancel = function() {
        var st = this.data.latest_exec.status; 
        return (st != "finalize" &&
                st != "cancel" &&
                st != "error"
               );
    };

    this.render = function() {
        var $container = $('<div class="jobFlow"></div>');
        var $list = $('<ol></ol>');
        $.each(this.data.jobs, function(index, thing) {

                   var priority = thing.priority;
                   var jobs = thing.jobs;
                   var status = app.job.getBucketStatus(jobs);

                   var $cell = $('<li class="ui-widget-content"></li>');
                   var $cellHdr = $('<ul></ul>');
                   $cellHdr.append(Util.makeListItem('Priority',priority));
                   $cellHdr.append(Util.makeListItem('Status',status));
                   $cell.append($cellHdr);
                   var $innerCell = $('<ul class="priorityList"></ul>');
                   $.each(jobs, function(idx, job){
                              var $jobEl = new Job(job).render();
                              var uri = job.uri;
                              // FIXIT need to nav to tpt!
                              $jobEl.click(function () {
                                               app.job.show(uri, 
                                                            app.getCenter());
                                           });
                              var $item = $('<li></li>');
                              $item.append($jobEl);
                              $innerCell.append($item);
                              $cell.append($innerCell);
                          });
                   $list.append($cell);
               });
        $container.append($list);

        return $container;
    };
};

function Job(data){
    this.data = data;

    this.render = function() {
        var job = this.data;
        var $el = $('<div id="job-'+job._id+
                       '"></div>');
        $el.addClass('ui-corner-all'); 
        $el.addClass('ui-widget-content');
        $el.addClass('job');
        
        var $hdr = $('<div class="ui-widget-header"></div>').html(job.NAME);
        $el.append($hdr);

        var $content = $('<div></div>');
        $content.addClass('content');
        var $list = $('<ul></ul>');
        $list.append(Util.makeListItem('Title',job.TITLE));
        $list.append(Util.makeListItem('Detail', job.DETAIL));
        $list.append(Util.makeListItem('Status', job.latest_exec.status));
        $content.append($list);

        $el.append($content);

        $el.addClass("status-"+this.getStatus(job));
        
        return $el;
    };

    this.getStatus = function() {
        if (this.data.latest_exec)
            return this.data.latest_exec.status;
        return null;
    };
};

function Tpt(data) {
    this.data = data;

    this.render = function() {
        var $body = $('<div"></div>');
        var $cfg = $('<div class="configs ui-widget-content">Configs</div>');
        var $cfgEl = this.buildTargets(this.data.CONFIG, 
                                       this.getConfigPath());
        $cfg.append($cfgEl);

        var $inp = $('<div class="inputs ui-widget-content">Inputs</div>');
        var $inputs = this.buildTargets(this.data.INPUT);
        $inp.append($inputs);

        var $out = $('<div class="outputs ui-widget-content">Outputs</div>');
        var $outputs = this.buildTargets(this.data.OUTPUT);
        $out.append($outputs);

        $body.append($cfg);
        $body.append($inp);
        $body.append($out);

        return $body;
    };

    this.getConfigPath = function() {
        return Util.dirName(this.data.script_path)+"/"+
            this.data.TSTRELPATH+"/configs";
    };

    this.buildTargets = function(root, path) {
        var objs = this.getNodes(root);
        var $container = $('<div></div>');
        var $list = $('<ul></ul>');
        $.each(objs,function(index, obj) {
                   var $li = $('<li></li>');
                   if (path) {
                       $li.append(Util.linkify(path+"/"+obj));
                   }
                   else {
                       $li.append(obj);
                   }
                   $list.append($li);
               });
        $container.append($list);
        return $container;
    };

    this.getNodes = function(root) {
        var dir = root.DIR;
        return this.walkNodes(root.DATA);
    };

    this.walkNodes = function(node) {
        var xs = [];
        for (key in node) {
            var val = node[key];
            if (val.FILE && val.DIR)
            {
                xs.push(val.DIR+"/"+val.FILE);
            }
            else
            {
                if (!Util.actuallyString(val)) {
                    var vals = this.walkNodes(val);
                    xs.concat(vals);
                }
            }
        }
        return xs;
    };

};

function Util() { };

// thanks be to crockford
Util.typeOf = function(value) {
    var s = typeof value;
    if (s === 'object') {
        if (value) {
            if (value instanceof Array) {
                s = 'array';
            }
            if (Util.actually_string(value)) {
                s = 'string';
            }
        } else {
            s = 'null';
        }
    }
    return s;
};

Util.listify = function(obj) {
   
    var type = Util.typeOf(obj);
    if (type === 'array')
    {
        return '<span class="value">'+obj.join(",")+'</span>';
    }
    else if (type === 'object')
    {
        var $listEl = $('<ul></ul>');
        for (var key in obj)
        {
            var $item = $('<li></li>');
            $item.append('<span class="key">'+key+': </span>');
            $item.append(Util.listify(obj[key]));
            $listEl.append($item);            
        }
        return $listEl;
    }
    else
    {
        if (obj == null)
            return '<span class="error">null</span>';
        else
            return '<span class="value">'+obj.toString()+'</span>';            
    }
};

Util.actually_string = function(obj){
    for (var key in obj)
    {
        if ((key == 0 || key == "0") && obj[key].length == 1)
            return true;
        else
            return false;
    }
    return false;
};

Util.makeListItem = function(key, val){
    var $item = $('<li></li>');
    $item.append('<span class="key">'+key+': </span>');        
    $item.append(val);
    return $item;
};

Util.getRFC822Date = function(oDate) {
      
    var aMonths = new Array("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                            "Jul", "Aug", "Sep", "Oct", "Nov", "Dec");
    
    var aDays = new Array( "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat");
    var dtm = new String();
  
    dtm = aDays[oDate.getDay()] + ", ";
    dtm += Util.padWithZero(oDate.getDate()) + " ";
    dtm += aMonths[oDate.getMonth()] + " ";
    dtm += oDate.getFullYear() + " ";
    dtm += Util.padWithZero(oDate.getHours()) + ":";
    dtm += Util.padWithZero(oDate.getMinutes()) + ":";
    dtm += Util.padWithZero(oDate.getSeconds()) + " " ;
    dtm += Util.getTZOString(oDate.getTimezoneOffset());
    return dtm;
  };

Util.padWithZero = function(val) {
    if (parseInt(val) < 10)
    {
        return "0" + val;
    }
    return val;
};

Util.getTZOString = function(timezoneOffset) {
    var hours = Math.floor(timezoneOffset/60);
    var modMin = Math.abs(timezoneOffset%60);
    var s = new String();
    s += (hours > 0) ? "-" : "+";
    var absHours = Math.abs(hours);
    s += (absHours < 10) ? "0" + absHours :absHours;
    s += ((modMin == 0) ? "00" : modMin);
    return(s);
};

Util.formatDate = function(date) {
    return (new Date(date).toLocaleString());
};

Util.summarize = function(uri) {
    var idx = uri.lastIndexOf("/")+1;
    return (uri.substring(0,idx)+"summary/"+uri.substring(idx));
};

Util.hostify = function(path) {
    var prefix = location.protocol+"//"+location.hostname;
    return path.replace("/mnt/output", prefix);
};

Util.linkify = function(path) {
    var uri = Util.hostify(path);
    return $('<a target="_new" href="'+uri+'">'+path+'</a>');
};

Util.actuallyString = function(obj){
    for (var key in obj)
    {
        if ((key == 0 || key == "0") && obj[key].length == 1)
            return true;
        else
            return false;
    }
    return false;
};

Util.dirName = function(path) {
    var toks = path.split("/");
    return (toks.slice(0, toks.length-1).join("/"));
};
