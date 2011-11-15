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
function View(controller) {

    this.controller = controller;

    this.makeTimeline = function($base, obj) {
        var $timeline = new Timeline(obj).render();
        $base.append($timeline);
        $timeline.dialog({autoOpen:false, 
                          title: 'Timeline',
                          width: 800,
                          show: 'slide',
                          closeOnEscape: true,
                          modal: true
                         });
        $base.click(function(){
                        $timeline.dialog('open');
                    });        
    };

    this.addNav = function(clazz, name, action, head) {
        var txt = name;
        if (head == null || head == false)
            txt = ' &gt; '+txt;

        var $lnk = $('<a class="breadcrumb" href="#">'+txt+'<a>');
        $lnk.click(action);
        var $trail = $('.breadcrumb-trail');
        this.cleanNav($trail, clazz);
        if (head != null && head == true)
            $trail.empty();
        $trail.append($('<li class="'+clazz+'"></li>').append($lnk));
    };

    this.cleanNav = function($trail, clazz) {
        switch (clazz) {
        case 'tst':
            var $tst = $trail.find('.tst');
            $tst.remove();
        case 'job':
            var $job = $trail.find('.job');
            $job.remove();
        case 'tpt':
            var $tpt = $trail.find('.tpt');
            $tpt.remove();
        }
    };
};

function TstView(controller) {
    this.controller = controller;

    this.list = function(tsts, $container) {
        $container.empty();

        $('#cancel-tst').hide();

        this.addNav('home','All Suites', 
                    function() {app.tst.list($container);}, true);

        var $table = $('<table width="100%"></table>');
        var $thead = $('<thead></thead>');
        var $header = $('<tr>');
        $header.append('<th>Name</th>');
        $header.append('<th>Status</th>');
        $header.append('<th>Time</th>');
        $thead.append($header);
        var $tbody = $('<tbody></tbody>');
        var view = this;
        $.each(tsts,function(index, obj) { 
                   $row = $('<tr>');
                   $row.append($('<td>').append(obj.NAME));
                   $row.append($('<td>').append(obj.latest_exec.status));
                   $row.append($('<td>').
                               append(Util.formatDate(
                                          obj.latest_exec.timestamp)));
                   var uri = obj.uri;
                   $row.click(function () {
                                  view.controller.show(uri, app.getCenter());
                               });
                   $tbody.append($row);
               });
        $table.append($thead);
        $table.append($tbody);
        $container.append($table);
        $table.dataTable(        
            {
		"bJQueryUI": true,
                "sPaginationType": "full_numbers",
                "aaSorting": [[2,'desc']],
                "iCookieDuration": 60*60*24
            });
    };

    this.show = function(tst, $container) {
        $container.empty();

        this.addNav('tst',
                    tst.NAME, 
                    function() {app.tst.show(tst.uri, $container);});

        var entity = new Tst(tst);
        if (entity.canCancel() && app.auth.isLoggedIn())
        {
            var $tgt = $('#cancel-tst');
            $tgt.show();
            $tgt.unbind('click');            
            $tgt.click(function() {
                           $.post(tst.uri, 
                                  JSON.stringify(
                                      {action: "cancel"}),
                                  function(data, stat){
                                      app.tst.list(app.getCenter());
                                  },
                                  "json");
                       });
            $tgt.confirm();
        }

        var $hdr = $('<div></div>');
        var $name = $('<h2></h2>');
        $name.append(tst.NAME);

        $hdr.append($name);
        var $detailList = $('<ul></ul>');
        var $repo = $('<a href="'+tst.pzm.repo_path+'" target="_new">'+
                      tst.pzm.repo_path+'</a>');
        $detailList.append(Util.makeListItem('PZM', $repo));
        var $user = $('<a href="mailto:'+tst.rfs.accessKey.contact_email+'">'+
                      tst.rfs.accessKey.contact_name+'</a>');
        $detailList.append(Util.makeListItem('User', $user)); 
        $detailList.append(Util.makeListItem('Details',tst.DETAIL));
        $detailList.append(Util.makeListItem('Uri',tst.uri));
        $detailList.append(Util.makeListItem('Script',tst.script_path));
        var $lnk = $('<a href="#">'+
                     tst.latest_exec.status +
                     ' ('+ Util.formatDate(tst.latest_exec.timestamp)+')'+
                     '</a>');
        var $curStat = Util.makeListItem('Status', $lnk);
                                         
        $detailList.append($curStat);
        this.makeTimeline($curStat, tst);

        $hdr.append($detailList);
        $container.append($hdr);
        
        var $body = new Tst(tst).render();
        $container.append($body);
    };
    
};
TstView.prototype = new View();

function JobView(controller) {
    this.controller = controller;

    this.show = function(job, $container) {
        $container.empty();

        $('#cancel-tst').hide();

        this.addNav('job',job.NAME, function() {app.job.show(job.uri, $container);});

        var $hdr = $('<div></div>');
        var $name = $('<h2></h2>');
        $name.append(job.NAME);

        $hdr.append($name);
        var $detailList = $('<ul></ul>');
        $detailList.append(Util.makeListItem('Details',job.DETAIL));
        $detailList.append(Util.makeListItem('Script',
                                             Util.linkify(job.script_path)));
        $detailList.append(Util.makeListItem('Uri',job.uri));
        var $lnk = $('<a href="#">'+
                     job.latest_exec.status +
                     ' ('+ Util.formatDate(job.latest_exec.timestamp)+')'+
                     '</a>');
        var $curStat = Util.makeListItem('Status', $lnk);
        $detailList.append($curStat);
        this.makeTimeline($curStat, job);

        $hdr.append($detailList);
        $container.append($hdr);

        var $table = $('<table width="100%"></table>');
        var $thead = $('<thead></thead>');
        var $header = $('<tr>');
        $header.append('<th>Name</th>');
        $header.append('<th>Status</th>');
        $header.append('<th>Time</th>');
        $thead.append($header);
        var $tbody = $('<tbody></tbody>');
        var view = this;
        $.each(job.tpts,function(index, obj) { 
                   $row = $('<tr>');
                   $row.append($('<td>').append(obj.NAME));
                   $row.append($('<td>').append(obj.latest_exec.status));
                   $row.append($('<td>').
                               append(Util.formatDate(
                                          obj.latest_exec.timestamp)));
                   var uri = obj.uri;
                   $row.click(function () {
                                  app.tpt.show(uri, app.getCenter());
                               });
                   $tbody.append($row);
               });
        $table.append($thead);
        $table.append($tbody);
        $('#busy').hide();
        $container.append($table);
        $table.dataTable(
            {
		"bJQueryUI": true,
                "sPaginationType": "full_numbers",
                "iCookieDuration": 60*60*24
            });
    };
    
};
JobView.prototype = new View();

function TptView(controller) {
    this.controller = controller;

    this.show = function(tpt, $container) {
        $container.empty();

        $('#cancel-tst').hide();

        this.addNav('tpt',tpt.NAME, 
                    function() {app.tpt.show(tpt.uri, $container);});

        var $hdr = $('<div></div>');
        var $name = $('<h2></h2>');
        $name.append(tpt.NAME);

        $hdr.append($name);
        var $detailList = $('<ul></ul>');
        var $lnk = $('<a href="#">'+
                     tpt.latest_exec.status +
                     ' ('+ Util.formatDate(tpt.latest_exec.timestamp)+')'+
                     '</a>');
        var $curStat = Util.makeListItem('Status', $lnk);
        $detailList.append($curStat);
        this.makeTimeline($curStat, tpt);
        $detailList.append(Util.makeListItem('Details',tpt.DETAIL));
        $detailList.append(Util.makeListItem('Script',
                                             Util.linkify(tpt.script_path)));
        if (tpt.result_dir) {
            $detailList.append(Util.makeListItem('Results', 
                                                Util.linkify(tpt.result_dir)));
        }
        $detailList.append(Util.makeListItem('Raw Data',
                                             Util.linkify(tpt.uri)));

        $hdr.append($detailList);
        $container.append($hdr);

        var $body = new Tpt(tpt).render();

        $container.append($body);
        
    };
};
TptView.prototype = new View();

function ClusterView() {
    
    this.update = function(data) {
        var c = this;
        $.each(data, function(index, node) {
                   if (node._id != "undefined") {
                       new Node(node).update();
                   }
               });
    };

    this.show = function(data) {
        var view = this;
        $.each(['windows', 'linux', 'mac'], function(idx, os){
                   ($('#hw-cluster .'+os)).empty();
               });
        $.each(['windows', 'linux', 'mac'], function(idx, os){
                   var flt = function(node) {
                       return node.OS.BRAND.toLowerCase().indexOf(os) >= 0;
                   };
            var cores = data['core.net'];
            if (cores) {
                view.arrayUpdate($('#hw-cluster .'+os), 
                                 cores.filter(flt), os);
            }
        });

        $.each(['linux'], function(idx, os){
                   ($('#grid-cluster .'+os)).empty();
               });
        $.each(['linux'], function(idx, os){
                   view.arrayUpdate($('#grid-cluster .'+os), 
                                    data['grid.net'], os);
               });
    };

    this.arrayUpdate = function($table, nodes, os){
        var view = this;
        $.each(nodes, function(idx, node){
                   $table
                       .append($('<tr>')
                               .append($('<td>')
                                       .append(new Node(node).render())
                                      )
                              );
               });
    };

    this.timeout = function() {
        var $timeout = 
            $('<div>Cluster monitoring halted due to inactivity</div>');
        
        $timeout.dialog({autoOpen:true, 
                         title: 'Monitoring Paused',
                         width: 800,
                         show: 'slide',
                         closeOnEscape: true,
                         modal: true,
			 buttons: 
                         {
			     'Ok': function() {
                                 monitorCluster();
				 $(this).dialog('close');
			     }
                         },
			 Cancel: function() {
                             monitorCluster();
			     $(this).dialog('close');
			 }
                        });
    };
};
ClusterView.prototype = new View();

