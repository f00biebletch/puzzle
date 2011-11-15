#
# Copyright 2011 Gianluca Filippini, Kevin McIntire
#
# Licensed under the Apache License, Version 2.0 (the "License"); you may not 
# use this file except in compliance with the License. You may obtain a copy 
# of the License at 
#
#    http://www.apache.org/licenses/LICENSE-2.0 
#
# Unless required by applicable law or agreed to in writing, software 
# distributed under the License is distributed on an "AS IS" BASIS, 
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. 
# See the License for the specific language governing permissions and 
# limitations under the License. 
#
package Job;

use strict;
use warnings;
use Exporter;
use Scalar::Util 'reftype';
use JSON;

use SysTool;
use SysCheck;
use JSONLog;

use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);


$VERSION     = 1.00;
@ISA         = qw(Exporter);

=head1 NAME

Job - Base implementation for a job (JOB) in Puzzle.

=head1 SYNOPSIS

Refer to any implemented perl PZM for usage.

=head1 DESCRIPTION

Job provides a base implementation for the lifecycle
methods of a job.

=head1 METHODS

=cut

=head2 info()

Base implementation of the info method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=item \%info

A hash ref to the INFO section from the job.

=back

=head3 Return

Standard JSON reply.

=cut
sub info {

    my %ans=();       # anser hash, empry by default
    my $quiet=0;      # FLAG : do we need to run quiet?
    my $keyfound=0;   # FLAG : did we found a valid result?
    #
    my $data;
    my $datakey;
    my $tmp;
    my $err = 0;

    my @cmd = split(/:/,$_[0]);
    my (%info) = %{$_[1]};

    ### Get the option for the CMD, if any.
    my $opt = getActionOpt(@cmd);
    if ($opt eq "q" ){
        $quiet=1;
    }

    ### Get the Command to perform on this action, if any.
    if (defined $cmd[1])
    {
        my $infokey; 
        my $cm = lc($cmd[1]);
        if ($cm eq "all")
        { 
            $keyfound=1;
            $ans{STATUS}="0";
            $ans{VALUE}=\%info;
        }
        elsif ($cm eq "list")
        {
            $keyfound=1;
            $ans{STATUS}="0";
            for $infokey ( sort keys %info ){
                $ans{VALUE}{$infokey}="1";
            }
        }
        # default selection
        else {
            for $infokey ( sort keys %info ){
                if ($cm eq lc($infokey)){
                    $keyfound=1;
                    $ans{STATUS}="0";
                    $ans{VALUE}{$infokey}=$info{$infokey};
                }
            }
        }
        ## return answer.
        if(!($keyfound)){
            $err+=1;
            $ans{VALUE}{ERROR}{$err}="unknown command";
        }
    }
    else{
        # called info action without any command
        # returning all infos
        $ans{STATUS}="0";
        $ans{VALUE}=\%info;
    }

    # SANITY CHECK : obj. info sintax
    # based on the API/obj version spec.
    #
    if(!($err)){
        if(exists ($info{NAME})){
            if($info{NAME} =~ /[@!#\$%\^&\*:;"'?<>~`\s\r\n\t]/i){
                $err+=1;
                $ans{VALUE}{ERROR}{$err}=
                    "obj info error : incorrect name syntax [$info{NAME}]";
            }
        }
        else{
            $err+=1;
            $ans{VALUE}{ERROR}{$err}="obj sintax error : name undefined";
        }

        # Check for fields required by Puzzle
        if (!defined $info{TSTRELPATH}) {
            $err+=1;
            $ans{VALUE}{ERROR}{$err}=
                "New() missing TSTRELPATH value in info: ".
                "FAILED.";
        }

        if (!defined $info{OUTPUT}{DIR}) {
            $err+=1;
            $ans{VALUE}{ERROR}{$err}=
                "New() missing OUTPUT.DIR value in info: ".
                "FAILED.";
        }

        if (!defined $info{PRIORITY}) {
            $err+=1;
            $ans{VALUE}{ERROR}{$err}=
                "New() missing PRIORITY value in info: ".
                "FAILED.";
        }

        #
        # check CONFIG->$KEY->FILE vector syntax
        #
        $data = \%info;
        for $datakey ( keys %{$data->{CONFIG}{DATA}} ) {
            if (exists $info{CONFIG}{DATA}{$datakey}{FILE}) {
                # retreive the array of file to check
                $tmp = $info{CONFIG}{DATA}{$datakey}{FILE};
                $tmp = reftype $tmp;
                # the FILE section must be an array, 
                # even if with a single vallue
                if (!($tmp =~ /^ARRAY/ )){
                    $err+=1;
                    $ans{VALUE}{ERROR}{$err}=
                        "New() syntax error in CONFIG : ".
                        "$tmp [$info{CONFIG}{DATA}{$datakey}{FILE}] : FAILED.";
                }
            }
            else{
                $err+=1;
                $ans{VALUE}{ERROR}{$err}=
                    "New() missing DATA->FILE value in CONFIG [$tmp] : FAILED.";
            }
        }
        #
        # check INPUT->$KEY->FILE vector syntax
        #
        for $datakey ( keys %{$data->{INPUT}{DATA}} ) {
            if (exists $info{INPUT}{DATA}{$datakey}{FILE}) {
                # retreive the array of file to check
                $tmp = $info{INPUT}{DATA}{$datakey}{FILE};
                $tmp = reftype $tmp;
                # the FILE section must be an array, 
                # even if with a single vallue
                if (!($tmp =~ /^ARRAY/ )){
                    $err+=1;
                    $ans{VALUE}{ERROR}{$err}="New() syntax error in INPUT : ".
                        "$tmp [$info{INPUT}{DATA}{$datakey}{FILE}] : FAILED.";
                }
            }
            else{
                $err+=1;
                $ans{VALUE}{ERROR}{$err}=
                    "New() missing DATA->FILE value in OUTPUT [$tmp] : FAILED.";
            }
        }
        #
        # check OUTPUT->$KEY->FILE vector syntax
        #
        # FIXIT 3 of these in a row!!!
        for $datakey ( keys %{$data->{OUTPUT}{DATA}} ) {
            if (exists $info{OUTPUT}{DATA}{$datakey}{FILE}) {
                # retreive the array of file to check
                $tmp = $info{OUTPUT}{DATA}{$datakey}{FILE};
                $tmp = reftype $tmp;
                # the FILE section must be an array, 
                # even if with a single vallue
                if (!($tmp =~ /^ARRAY/ )){
                    $err+=1;
                    $ans{VALUE}{ERROR}{$err}="New() syntax error in OUTPUT : ".
                        "$tmp [$info{OUTPUT}{DATA}{$datakey}{FILE}] : FAILED.";
                }

            }
            else{
                $err+=1;
                $ans{VALUE}{ERROR}{$err}=
                    "New() missing DATA->FILE value in OUTPUT [$tmp] : FAILED.";
            }
        }

        #
        # more syntax check t.b.d
        #
        # ....
        # ....
    }

    # Fucntion Call return
    # NOTE : we always return an hash
    #        with the answer inside.
    #        The answer is coming from the previous
    #        code. We just need to add the status.
    # NOTE : The function return one hash only.
    #        Also, answer can be empty.
    if($err){
        $err+=1;
        $ans{STATUS}="-1";
        $ans{VALUE}{ERROR}{$err}="error in action cmd exec.";
    }
    else{
        $ans{STATUS}="0";
    }
    JSONLog("jTXT",\%ans) if (!$quiet);
    # always return an hash
    return \%ans;
}

=head2 workflow()

Base implementation of the workflow method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=item $Bin

The location of perl execution context.

=back

=head3 Return

Standard JSON reply.

=cut
sub workflow{

    my @cmd = split(/:/,$_[0]);
    my $Bin = $_[1];

    my %ans=();       # anser hash, empry by default
    my $quiet=0;      # FLAG : do we need to run quiet?
    my $keyfound=0;   # FLAG : did we found a valid result?
    #
    my $task;
    my $taskdir = $Bin.'/tpt/';
    my @tasklist;
    my @filelist;
    my @descriptor;
    my %tasks;
    my $tmp;
    my $err = 0;

    ### Get the option for the CMD, if any.
    my $opt = getActionOpt(@cmd);
    if ($opt eq "q" ){
        $quiet=1;
    }

    opendir(IMD, $taskdir) || die("Cannot open directory");
    @filelist = readdir(IMD);
    closedir(IMD);

    #filter the filenames
    foreach $task (@filelist){
        if ( $task =~ /^tpt_(\w*).pl/i ){
            push(@tasklist, $task);

            # get the priority
            @descriptor = `$Bin/tpt/$task  -a info`;
            # check the status on $tmp[0]
            my $string = join('',@descriptor); chomp($string);
            $tmp = from_json($string);

            # now filter the results
            # for the worklist we want to know just the list
            # of virtual TPT that are going to be used
            # by the JOB to create configurations etc.
            #
            if(exists $tmp->{STATUS}){
                if(lc($tmp->{STATUS}) eq (lc("0"))){
                    if(exists $tmp->{VALUE}{PRIORITY} &&
                       exists $tmp->{VALUE}{TYPE} ) {
                        if( lc($tmp->{VALUE}{TYPE}) eq "real" ){
                            $tasks{$1}{PRIORITY}= $tmp->{VALUE}{PRIORITY};
                            #add the filename of the job
                            $tasks{$1}{FILE}=$task;
                        }
                    }
                    else{ 
                        $err+=1; 
                        $ans{VALUE}{ERROR}{$err}=
                            "workflow : undefined priority for [$task]";
                    }
                }
                else{ 
                    $err+=1;
                    $ans{VALUE}{ERROR}{$err}="workflow : error on TPT ".
                        "info request [$task]";
                }
            }
            else{ 
                $err+=1; 
                $ans{VALUE}{ERROR}{$err}="workflow : bad TPT found [$task]";
            }
        }
    }
    # Fucntion Call return
    # NOTE : we always return an hash
    #        with the answer inside.
    #        The answer is coming from the previous
    #        code. We just need to add the status.
    # NOTE : The function return one hash only.
    #        Also, answer can be empty.
    if($err){
        $err+=1;
        $ans{STATUS}="-1";
        $ans{VALUE}{ERROR}{$err}="error in worklist cmd exec.";
    }
    else{
        $ans{STATUS}="0";
        $ans{VALUE}=\%tasks
    }
    JSONLog("jTXT",\%ans) if (!$quiet);
    # always return an hash
    return \%ans;
}

=head2 worklist()

Base implementation of the worklist method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=item $Bin

The location of perl execution context.

=back

=head3 Return

Standard JSON reply.

=cut
sub worklist{

    my @cmd = split(/:/,$_[0]);
    my $Bin = $_[1];

    my %ans=();       # anser hash, empry by default
    my $quiet=0;      # FLAG : do we need to run quiet?
    my $keyfound=0;   # FLAG : did we found a valid result?
    #
    my $task;
    my $taskdir = $Bin.'/tpt/';
    my @tasklist;
    my @filelist;
    my @descriptor;
    my %tasks;
    my $tmp;
    my $err = 0;

    ### Get the option for the CMD, if any.
    my $opt = getActionOpt(@cmd);
    if ($opt eq "q" ){
        $quiet=1;
    }

    opendir(IMD, $taskdir) || die("Cannot open directory");
    @filelist = readdir(IMD);
    closedir(IMD);

    #filter the filenames
    foreach $task (@filelist){
        if ( $task =~ /^tpt_(\w*).pl/i ){
            push(@tasklist, $task);

            # get the priority
            @descriptor = `$Bin/tpt/$task  -a info`;
            # check the status on $tmp[0]
            my $string = join('',@descriptor); chomp($string);
            $tmp = from_json($string);

            # now filter the results
            # for the worklist we want to know just the list
            # of virtual TPT that are going to be used
            # by the JOB to create configurations etc.
            #
            if(exists $tmp->{STATUS}){
                if(lc($tmp->{STATUS}) eq (lc("0"))){
                    if(exists $tmp->{VALUE}{PRIORITY} &&
                       exists $tmp->{VALUE}{TYPE} ) {
                        if( lc($tmp->{VALUE}{TYPE}) eq "virtual" ){
                            $tasks{$1}{PRIORITY}= $tmp->{VALUE}{PRIORITY};
                            #add the filename of the job
                            $tasks{$1}{FILE}=$task;
                        }
                    }
                    else{ 
                        $err+=1; 
                        $ans{VALUE}{ERROR}{$err}="worklist : undefined ".
                            "priority for [$task]";
                    }
                }
                else{ 
                    $err+=1;
                    $ans{VALUE}{ERROR}{$err}="worklist : error on TPT ".
                        "info request [$task]";
                }
            }
            else{ 
                $err+=1; 
                $ans{VALUE}{ERROR}{$err}="worklist : bad TPT found [$task]";
            }
        }
    }
    # Fucntion Call return
    # NOTE : we always return an hash
    #        with the answer inside.
    #        The answer is coming from the previous
    #        code. We just need to add the status.
    # NOTE : The function return one hash only.
    #        Also, answer can be empty.
    if($err){
        $err+=1;
        $ans{STATUS}="-1";
        $ans{VALUE}{ERROR}{$err}="error in worklist cmd exec.";
    }
    else{
        $ans{STATUS}="0";
        $ans{VALUE}=\%tasks
    }
    JSONLog("jTXT",\%ans) if (!$quiet);
    # always return an hash
    return \%ans;
}

=head2 new()

Base implementation for the new method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=item $Bin

The location of perl execution context.

=item \%info

A hash ref to the INFO section from the job.

=item \%env

A hash ref to the ENV section from the job.

=item \&callback()

A callback to generate individual test points.

=over 4

=item $root

The absolute path of the job.

=item $worklist

The job worklist to use in generating the test points.

=item \%INFO

A hash ref to the INFO section from the job.

=item \%ENV

A hash ref to the ENV section from the job.

=back

=back

=head3 Return

Standard JSON reply.

=cut
sub new{

    my @cmd = split(/:/,$_[0]);
    my $Bin = $_[1];
    my (%INFO) = %{$_[2]};
    my (%ENV) = %{$_[3]};
    my $callback = $_[4];

    my %ans=();       # anser hash, empry by default
    my $quiet=0;      # FLAG : do we need to run quiet?
    #
    my %tmp;
    my $tmp;
    my $info={};
    my $worklist={};
    my $work;
    my $workflow={};
    my $tpt;
    my $infoCmd;
    my $logMsg;
    #
    my @data;
    my @descriptor;
    my $data;
    my $datakey;
    my $eventkey;
    my $tstabspath;
    my $jobabspath;
    #
    my $err = 0;

    ### Get the option for the CMD, if any.
    my $opt = getActionOpt(@cmd);
    if ($opt eq "q" ){
        $quiet=1;
    }
    # prepare foldernames
    $tstabspath = $Bin."/".$ENV{TSTRELPATH}."/";
    $jobabspath = $Bin."/".$ENV{JOBRELPATH}."/";

    # calling sanity check
    #
    if(lc($INFO{TYPE}) eq "virtual"){
        # virtual JOB cannot do more than info/worklist/workflow
        $err+=1;
        $ans{VALUE}{ERROR}{$err}="virtual JOB, cannot call New.";
    }
    else{
        # check the INFO->OS section
        #
        if(exists $INFO{OS}{CHECK}){
            $tmp{OS}=$INFO{OS};
            if (!(CheckOS(\%tmp))){
                $err+=1;
                $ans{VALUE}{ERROR}{$err}=
                    "HostOS does not match the JOB requested OS";
            }
        }
        # check the INFO->HW type
        #
        if(exists $INFO{HW}{CHECK}){
            $tmp{HW}=$INFO{HW};
            if (!(CheckHW(\%tmp))){
                $err+=1;
                $ans{VALUE}{ERROR}{$err}=
                    "HostHW does not match the JOB requested HW.";
            }
        }
        # read the info of this JOB
        # we read calling the function to avoid
        # syntax check twice
        $infoCmd = "info:all:q";
        # get the pointer to the data struct.
        $info = info( $infoCmd, \%INFO );
        # sanity check

        if (defined $info->{STATUS})
        {
            if($info->{STATUS} ne "0" ){
                $err+=1;
                $ans{VALUE}{ERROR}{$err}="New() info parsing : FAILED.";
            }
            else{
                # create the JOB output folder if needed
                if (!(-d $ENV{OUTPUT}{PATH})){
                    mkdir $ENV{OUTPUT}{PATH};
                }
                # now double check permissions
                if (!(-d $ENV{OUTPUT}{PATH})){
                    $err+=1;
                    $ans{VALUE}{ERROR}{$err}=
                        "New() unable to create output dir : FAILED.";
                }
                else{
                    chmod 0754, $ENV{OUTPUT}{PATH};
                }
                # check if the LogFile it's already there
                $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
                if(!(-f $tmp)){
                    open(LOGFILE, ">$tmp");
                    print LOGFILE "Name   : ".$ENV{NAME}."\n";
                    print LOGFILE "Title  : ".$ENV{TITLE}."\n";
                    print LOGFILE "Detail : ".$ENV{DETAIL}."\n";
                    close(LOGFILE);
                }
                # the logfile must be there now.
                if(-f $tmp){
                    # check if there is a previous run in the logfile
                    if(logFileParse($tmp,"New") ||
                       logFileParse($tmp,"Run") ||
                       logFileParse($tmp,"Finalize") )
                    {
                        if(logFileParse($tmp,"Finalize , FINISH")){
                            # previous pass is fine, epmty out the log file
                            open(LOGFILE, ">$tmp");
                            $logMsg = "Name   : ".$ENV{NAME}."\n".
                                "Title  : ".$ENV{TITLE}."\n"."Detail : ".
                                $ENV{DETAIL}."\n";
                            logFileDump($tmp,$logMsg);
                            logFileDump($tmp,"\n#==========================".
                                        "=============================\n");
                            my $jobj = 
                                JSON->new->utf8(1)->pretty(1)->encode(\%ENV);
                            $jobj=$jobj; logFileDump($tmp,$jobj);
                            logFileDump($tmp,"\n#==========================".
                                        "=============================\n");
                            close(LOGFILE);
                            $logMsg = "New , CREATE , ".getDateTimeNow();
                            logFileDump($tmp,$logMsg);
                        }
                        else{
                            $err+=1;
                            $ans{VALUE}{ERROR}{$err}=
                                "New() previous JOB not finalized : FAILED.";
                        }
                    }
                    else{
                        # no previous passes in the logFile
                        $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
                        open(LOGFILE, ">$tmp");
                        $logMsg = "Name   : ".$ENV{NAME}."\n"."Title  : ".
                            $ENV{TITLE}."\n"."Detail : ".$ENV{DETAIL}."\n";
                        logFileDump($tmp,$logMsg);
                        logFileDump($tmp,"\n#==============================".
                                    "=========================\n");
                        my $jobj = JSON->new->utf8(1)->pretty(1)->encode(\%ENV);
                        $jobj=$jobj; logFileDump($tmp,$jobj);
                        logFileDump($tmp,"\n#==============================".
                                    "=========================\n");
                        close(LOGFILE);
                        $logMsg = "New , CREATE , ".getDateTimeNow();
                        logFileDump($tmp,$logMsg);
                    }
                }
                else{
                    $err+=1;
                    $ans{VALUE}{ERROR}{$err}=
                        "New() unable to create log file : FAILED.";
                }
                # create the sandbox folder if needed
                if (!(-d $ENV{SANDBOX}{PATH})){
                    mkdir $ENV{SANDBOX}{PATH};
                }
                # now double check permissions
                if (!(-d $ENV{SANDBOX}{PATH})){
                    $err+=1;
                    $ans{VALUE}{ERROR}{$err}="New() unable to create sandbox dir : FAILED.";
                }
                else{
                    chmod 0754,$ENV{SANDBOX}{PATH};
                }
                
                ##
                ## CHECK : PRE section
                ##         we loop check on configs / input / output to match
                ##         test conditions from the $INFO section.
                ##
                ## t.b.d
                if(!($err)){
                    # update logFile
                    $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
                    $logMsg = "New , START, ".getDateTimeNow();
                    logFileDump($tmp,$logMsg);
                    
                    # loop over the $INFO{CONFIG}{KEYS} and check presence
                    # Note: the loop is performed on the $INFO section but the
                    # check is done on the $ENV section because it's a real JOB,
                    # and a real JOB is supposed to have the $ENV correct.
                    #
                    if (exists $info->{VALUE}{CONFIG}{DATA}){
                        for $datakey ( keys %{$info->{VALUE}{CONFIG}{DATA}} ) {
                            # check for presence requested??
                            if (exists 
                                $info->{VALUE}{CONFIG}{DATA}{$datakey}{CHECK}{NEW}{PRE}{PRESENCE}){
                                if (exists $info->{VALUE}{CONFIG}{DATA}{$datakey}{FILE}){
                                    #
                                    # the "all" option is t.b.s
                                    # retreive the array of file to check
                                    @data = 
                                        @{$ENV{CONFIG}{DATA}{$datakey}{FILE}};
                                    for $data ( @data ){
                                      
                                        $tmp = 
                                            $ENV{CONFIG}{DATA}{$datakey}{PATH}."/".$data;
                                       
                                        if (!(-f $tmp)){
                                            $err+=1;
                                            $ans{VALUE}{ERROR}{$err}="New() ".
                                                "missing FILE in CONFIG ".
                                                "[$tmp] : FAILED.";
                                            }
                                        }
                                    }
                                }
                                else{
                                    $err+=1;
                                    $ans{VALUE}{ERROR}{$err}="New() missing ".
                                        "DATA->FILE value in CONFIG [$tmp] : ".
                                        "FAILED.";
                                }
                            }
                        }
                    

                    # loop over the $INFO{INPUT}{KEYS} and check presence
                    # Note: the loop is performed on the $INFO section but the
                    # check is done on the $ENV section because it's a real JOB,
                    # and a real JOB is supposed to have the $ENV correct.
                    #
                    if (exists $info->{VALUE}{INPUT}{DATA} ){
                        for $datakey ( keys %{$info->{VALUE}{INPUT}{DATA}} ) {
                            # check for presence requested??
                            if (exists $info->{VALUE}{INPUT}{DATA}{$datakey}{CHECK}{NEW}{PRE}{PRESENCE}){
                                if (exists $info->{VALUE}{INPUT}{DATA}{$datakey}{FILE}){
                                    #
                                    # the "all" option is t.b.s
                                    # retreive the array of file to check
                                    @data = 
                                        @{$ENV{INPUT}{DATA}{$datakey}{FILE}};
                                    for $data ( @data ){
                                        $tmp = 
                                            $ENV{INPUT}{DATA}{$datakey}{PATH}.
                                            "/".$data;
                                        if (!(-f $tmp)){
                                            $err+=1;
                                            $ans{VALUE}{ERROR}{$err}="New() ".
                                                "missing FILE in INPUT [$tmp]".
                                                " : FAILED.";
                                        }
                                    }
                                }
                                else{
                                    $err+=1;
                                    $ans{VALUE}{ERROR}{$err}="New() missing ".
                                        "DATA->FILE value in INPUT [$tmp] : ".
                                        "FAILED.";
                                }
                            }
                        }
                    }

                    # FIXIT again recurring code!!! refactor!
                    # loop over the $INFO{OUTPUT}{KEYS} and check presence
                    # Note: the loop is performed on the $INFO section but the
                    # check is done on the $ENV section because it's a real JOB,
                    # and a real JOB is supposed to have the $ENV correct.
                    #
                    if( exists $info->{VALUE}{OUTPUT}{DATA}){
                        for $datakey ( keys %{$info->{VALUE}{OUTPUT}{DATA}} ) {
                            # check for presence requested??
                            if (exists $info->{VALUE}{OUTPUT}{DATA}{$datakey}{CHECK}{NEW}{PRE}{PRESENCE}){
                                if (exists $info->{VALUE}{OUTPUT}{DATA}{$datakey}{FILE}){
                                    #
                                    # the "all" option is t.b.d
                                    # retreive the array of file to check
                                    @data = 
                                        @{$ENV{OUTPUT}{DATA}{$datakey}{FILE}};
                                    for $data ( @data ){
                                        $tmp = 
                                            $ENV{OUTPUT}{DATA}{$datakey}{PATH}.
                                            "/".$data;
                                        if (!(-f $tmp)){
                                            $err+=1;
                                            $ans{VALUE}{ERROR}{$err}="New() ".
                                                "missing FILE in OUTPUT ".
                                                "[$tmp] : FAILED.";
                                        }
                                    }
                                }
                                else{
                                    $err+=1;
                                    $ans{VALUE}{ERROR}{$err}="New() missing ".
                                        "DATA->FILE value in OUTPUT [$tmp] : ".
                                        "FAILED.";
                                }
                            }
                        }
                    }
                    
                    # loop over the $ENV{EXEBIN}{KEYS} we ALWAYS need to check their presence on New()
                    #
                    if(exists $info->{VALUE}{EXEBIN}{DATA}){
                        for $datakey ( keys %{$info->{VALUE}{EXEBIN}{DATA}} ) {
                            if(exists ($INFO{EXEBIN}{DATA}{$datakey}{CHECK})){
                                $tmp = 
                                    $tstabspath."../bin/".
                                    $ENV{EXEBIN}{$datakey}{CMD};
                                if (!(-f $tmp)){
                                    $err+=1;
                                    $ans{VALUE}{ERROR}{$err}="New() unable ".
                                        "to find exebin [$tmp] : FAILED.";
                                }
                            }
                        }
                    }

                    # field check for the INFO->RESOURCES section
                    #
                    # field check for INFO->SYSBIN section
                    #
                    # ...
                    # ... other checks
                    #
                }
            }
        }
        else{
            $err+=1;
            # info is not even defined, something failed.
            $ans{VALUE}{ERROR}{$err}="New() info parsing : FAILED.";
        }

        ## Job.New() : CORE SECTION
        ## This is the part that have to be manually customized to build the
        ## right workflow of the job, based on tpt specific definition.
        ##   <- for each virtual tpt in the worklist create real tpt files
        ##    -> build workflow.
        ##   <- for each real tpt in the workflow call TPT.New()
        ##
                
        # get the worklist of this job
        $worklist = worklist( "worklist:all:q", $Bin);

        # sanity check
        if (defined $worklist->{STATUS})
        {
            if(lc($worklist->{STATUS}) eq "0"){
                my $resl = $callback->($jobabspath, $worklist->{VALUE},
                                       \%INFO, \%ENV);
                my %res = %{$resl};
                if ($res{STATUS} ne "0")
                {
                    $err += 1;
                    # FIXIT copy all errors??
                    $ans{VALUE}{ERROR}{$err}=$res{VALUE}{ERROR}{"0"};
                }
            }
            else{ 
                $err+=1;
                $ans{VALUE}{ERROR}{$err}="New() [$infoCmd] : FAILED.";
            }
        }
        else{
            $err+=1;
            # worklist is not even defined, something failed.
            $ans{VALUE}{ERROR}{$err}="New() worklist parsing : FAILED.";
        }

        ##
        ## CORE SECTION : CALL TPT.New() for each real TPT in the workflow
        ##
        if (!($err)){
            # update logfile
            $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
            $logMsg = "New , calling workflow()";
            logFileDump($tmp,$logMsg);
            #
        
            # get the pointer to the data struct.
            $workflow = workflow("workflow:all:q" , $Bin);
        
            # sanity check
            if (defined $workflow->{STATUS})
            {
                if(lc($workflow->{STATUS}) eq "0"){
                    for $work ( sort { $workflow->{VALUE}{$a}{PRIORITY} cmp 
                                           $workflow->{VALUE}{$b}{PRIORITY} } 
                                keys %{$workflow->{VALUE}} ){
                        # call the New() of this TPT
                        $infoCmd = $jobabspath."/tpt/tpt_".$work.".pl -a new";
                        # update logfile
                        $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
                        $logMsg = "New , calling TPT.New() , ".$infoCmd;
                        logFileDump($tmp,$logMsg);
                        #
                        @descriptor = `$infoCmd`;
                        # check the status on $tmp[0]
                        my $string = join('',@descriptor); 
                        chomp($string);
                        $tmp = from_json($string);
                        if(exists $tmp->{STATUS}){
                            if(lc($tmp->{STATUS}) ne (lc("0"))){
                                $err+=1;
                                $ans{VALUE}{ERROR}{$err}=
                                    "TPT.New() error on [$work] : FAILED.";
                            }
                        }
                        else{ 
                            $err+=1; 
                            $ans{VALUE}{ERROR}{$err}=
                                "TPT.New() on [$work] : UNDEFINED.";
                        }
                    }
                }
                else{ 
                    $err+=1; 
                    $ans{VALUE}{ERROR}{$err}="Workflow request : FAILED.";
                }
            }
            else{ 
                $err+=1; 
                $ans{VALUE}{ERROR}{$err}="Workflow request : UNDEFINED.";
            }
        }
    }

    
    # Fucntion Call return
    # NOTE : we always return an hash
    #        with the answer inside.
    #        The answer is coming from the previous
    #        code. We just need to add the status.
    # NOTE : The function return one hash only.
    #        Also, answer can be empty.
    if($err){
        $err+=1;
        $ans{STATUS}="-1";
        $ans{VALUE}{ERROR}{$err}="unknown command or internal error";
        $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
        $logMsg = "New , FAILED , ".getDateTimeNow();
        logFileDump($tmp,$logMsg);
    }
    else{
        # ALL looks good
        $ans{STATUS}="0";
        $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
        $logMsg = "New , FINISH , ".getDateTimeNow();
        logFileDump($tmp,$logMsg);
    }
    JSONLog("jTXT",\%ans) if (!$quiet);
    # always return an hash
    return \%ans;
}

=head2 run()

Base implementation of the run method; simply runs each tpt.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=item $Bin

The location of perl execution context.

=item \%INFO

A hash ref to the INFO section from the job.

=item \%ENV

A hash ref to the ENV section from the job.

=back

=head3 Return

Standard JSON reply.

=cut
sub run{

    my @cmd = split(/:/,$_[0]);
    my $Bin = $_[1];
    my (%INFO) = %{$_[2]};
    my (%ENV) = %{$_[3]};

    my %ans=();       # anser hash, empry by default
    my $quiet=0;      # FLAG : do we need to run quiet?
    my $tptselect;
    my $cmd="";
    #
    my $workflow={};
    my $infoCmd;
    my @descriptor;
    my $tmp;
    my $work;
    my $logMsg;
    #
    my $tstabspath;
    my $jobabspath;

    my $err = 0;
    #

    ### Get the option for the CMD, if any.
    $cmd = getActionCmd(@cmd);

    ### Get the option for the CMD, if any.
    my $opt = getActionOpt(@cmd);
    if ($opt eq "q" ){
        $quiet=1;
    }

    # single selection requested?
    if((lc($cmd) eq "tpt") && (lc($opt) ne "")){
        $tptselect = $opt unless ($quiet);
    }
    else{
        undef $tptselect;
    }

    # prepare foldernames
    $tstabspath = $Bin."/".$ENV{TSTRELPATH}."/";
    $jobabspath = $Bin."/".$ENV{JOBRELPATH}."/";

    # calling sanity check
    #
    if(lc($INFO{TYPE}) eq "virtual"){
        $err+=1;
        $ans{VALUE}{ERROR}{$err}="virtual TPT, cannot call Run.";
    }
    else{
        # check for sandbox
        #
        if(defined($ENV{SANDBOX}{PATH})){
            $tmp = $ENV{SANDBOX}{PATH};
            mkdir $tmp unless(-d $tmp);
            # check permissions
            if(-d $tmp){
                chmod 0754, $tmp;
            }
            else{
                $err+=1;
                $ans{VALUE}{ERROR}{$err}="unable to create sandbox.[$tmp]";
            }
        }
        # check for previous New() call result
        #
        $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
        if(!(logFileParse($tmp,"New , FINISH"))){
            $err+=1;
            $ans{VALUE}{ERROR}{$err}="Run : TPT not intitialized correctly.";
        }
        else{
            ##
            ## CHECK : PRE section
            ##         we loop check on configs / input / output to match
            ##         test conditions from the $INFO section.
            ##
            ## t.b.d
            # check the ENV->SYSBIN section
            #
            # check the ENV->CONFIG section
            #
            # check the ENV->INPUT section
            #
            # check the ENV->OUTPUT section
            # ...

            ##
            ## CORE SECTION : CALL TPT.Run() for each real TPT in the workflow
            ## The order is very important.
            ##
            if (!($err)){
                # LogFile update
                $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
                $logMsg = "Run , START , ".getDateTimeNow();
                logFileDump($tmp,$logMsg);

                # get the workflow of this job
                $workflow = workflow("workflow:all:q", $Bin);

                # sanity check
                if (defined $workflow->{STATUS})
                {
                    if(lc($workflow->{STATUS}) eq "0"){
                        for $work ( 
                            sort { $workflow->{VALUE}{$a}{PRIORITY} cmp 
                                       $workflow->{VALUE}{$b}{PRIORITY} } 
                            keys %{$workflow->{VALUE}} ){
                            if( defined $tptselect ){
                                if(lc($work) eq lc($tptselect)){
                                    # call the New() of this TPT
                                    $infoCmd = $jobabspath."/tpt/tpt_".$work.
                                        ".pl -a run";
                                    # log file update
                                    $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.
                                        ".log";
                                    $logMsg = "Run, cmd, ".$infoCmd;
                                    logFileDump($tmp,$logMsg);
                                    #
                                    @descriptor = `$infoCmd`;
                                    # check the status on $tmp[0]
                                    my $string = join('',@descriptor); 
                                    chomp($string);
                                    $tmp = from_json($string);
                                    if(exists $tmp->{STATUS}){
                                        if(lc($tmp->{STATUS}) ne (lc("0"))){
                                            $err+=1;
                                            $ans{VALUE}{ERROR}{$err}=
                                                "TPT.Run() error on [$work] ".
                                                ": FAILED.";
                                        }
                                    }
                                    else{
                                        $err+=1; 
                                        $ans{VALUE}{ERROR}{$err}=
                                            "TPT.Run() on [$work] : UNDEFINED.";
                                    }
                                }# tptselect does not match : skip
                            }
                            else{
                                # call the Run() of this TPT
                                $infoCmd = $jobabspath."/tpt/tpt_".$work.
                                    ".pl -a run";
                                # log file update
                                $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
                                $logMsg = "Run, cmd, ".$infoCmd;
                                logFileDump($tmp,$logMsg);
                                #
                                @descriptor = `$infoCmd`;
                                # check the status on $tmp[0]
                                my $string = join('',@descriptor); 
                                chomp($string);
                                $tmp = from_json($string);
                                if(exists $tmp->{STATUS}){
                                    if(lc($tmp->{STATUS}) ne (lc("0"))){
                                        $err+=1;
                                        $ans{VALUE}{ERROR}{$err}=
                                            "TPT.Run() error on [$work] : ".
                                            "FAILED.";
                                    }
                                }
                                else{
                                    $err+=1; 
                                    $ans{VALUE}{ERROR}{$err}="TPT.Run() on ".
                                        "[$work] : UNDEFINED.";
                                }
                            }#$tptselect undefined or all.
                        }
                    }
                    else{ 
                        $err+=1; 
                        $ans{VALUE}{ERROR}{$err}="Workflow request : FAILED.";}
                }
                else{ 
                    $err+=1; 
                    $ans{VALUE}{ERROR}{$err}="Workflow request : UNDEFINED.";}
            }

            ##
            ## CHECK : POST section
            ##         we loop check on configs / input / output to match
            ##         test conditions from the $INFO section.
            ##
            if(!($err)){
                # check if I need to signal to SCATTER
                # based on %INFO{OUTPUT}{DATA} section
            }
        }
    }

    # any errors?
    if (!($err)){
        $ans{STATUS}="0";
        $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
        $logMsg = "Run , FINISH , ".getDateTimeNow();
        logFileDump($tmp,$logMsg);
    }
    else{
        $err+=1;
        $ans{STATUS}="-1";
        $ans{VALUE}{ERROR}{$err}="Run : FAILED.";
        $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
        $logMsg = "Run , FAILED , ".getDateTimeNow();
        logFileDump($tmp,$logMsg);
    }

    # return
    JSONLog("jTXT",\%ans) if (!$quiet);
    # always return an hash
    return \%ans;
}

=head2 finalize()

Base implementation for the finalize method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=item $Bin

The location of perl execution context.

=item \%info

A hash ref to the INFO section from the job.

=item \%env

A hash ref to the ENV section from the job.

=item \&callback()

A callback to finalize the job; takes same args as this method.

=back

=head3 Return

Standard JSON reply.

=cut
sub finalize{

    my @cmd = split(/:/,$_[0]);
    my $Bin = $_[1];
    my (%INFO) = %{$_[2]};
    my (%ENV) = %{$_[3]};
    my $callback = $_[4];

    my %ans=();       # anser hash, empry by default
    my $quiet=0;      # FLAG : do we need to run quiet?
    #
    my $tmp;
    my $logMsg;
    my $err = 0;

    ### Get the option for the CMD, if any.
    my $opt = getActionOpt(@cmd);
    if ($opt eq "q" ){
        $quiet=1;
    }

    # sanity check
    if (lc($INFO{TYPE}) eq "virtual"){
        # virtual TPT cannot control input
        $err+=1;
        $ans{VALUE}{ERROR}{$err}="virtual JOB, cannot Finalize";
    }
    else{
        # LogFile check
        $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
        if(!(-f $tmp)){
            $err+=1;
            $ans{VALUE}{ERROR}{$err}="Finalize : missing logfile.";
        }
        else{
            # check for previous Run() call result
            #
            $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
            if(!(logFileParse($tmp,"Run , FINISH"))){
                $err+=1;
                $ans{VALUE}{ERROR}{$err}="Finalize: JOB did not Run correctly.";
            }
            else{
                # LogFile update
                $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
                $logMsg = "Finalize , START , ".getDateTimeNow();
                logFileDump($tmp,$logMsg);

                # add code here
                ##
                ## CHECK : PRE section
                ##         we loop check on configs / input / output to match
                ##         test conditions from the $INFO section.
                ##
                if(!($err)){
                    if (defined $callback)
                    {
                        $callback->(@_);
                    }
                }

                ##
                ## Finalize : Core Code Section
                ##
                if(!($err)){
                    ## t.b.d
                    # add the code to perform check on input/output section
                    # also for gaher/scatter messaging.
                    # ...
                    # ...
                    # sending a msg to the ctrl if Puzzle is running.
                    # ...
                    # ...
                }
            }

            ##
            ## CHECK : POST section
            ##         we loop check on configs / input / output to match
            ##         test conditions from the $INFO section.
            ##
            if(!($err)){
                ## t.b.d
            }
        }
    }

    # any errors?
    if (!($err)){
        $ans{STATUS}="0";
        # LogFile update
        $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
        $logMsg = "Finalize , FINISH , ".getDateTimeNow();
        logFileDump($tmp,$logMsg);
    }
    else{
        $err+=1;
        $ans{STATUS}="-1";
        $ans{VALUE}{ERROR}{$err}="TPT.Finalized : FAILED.";
        # LogFile update
        $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
        $logMsg = "Finalize , FAILED , ".getDateTimeNow();
        logFileDump($tmp,$logMsg);
    }

    # remove sandbox no matter what.
    $tmp = $ENV{SANDBOX}{PATH};
    if(-d $tmp){
        $tmp = $ENV{SYSBIN}{RM}{CMD}." -fr ".$ENV{SANDBOX}{PATH};
        system($tmp);
    }

    # return 
    JSONLog("jTXT",\%ans) if (!$quiet);
    # always return an hash
    return \%ans;
}

=head1 COPYRIGHT

Copyright 2011 Gianluca Filippini, Kevin McIntire
All Rights Reserved

=cut

1;
