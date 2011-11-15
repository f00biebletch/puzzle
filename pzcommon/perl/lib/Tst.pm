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
package Tst;

use strict;
use warnings;
use Exporter;
use Scalar::Util 'reftype';
use JSON;

use SysTool;
use JSONLog;

use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

$VERSION     = 1.00;
@ISA         = qw(Exporter);

=head1 NAME

Tst - Base implementation for a test suite (TST) in Puzzle.

=head1 SYNOPSIS

Refer to any implemented perl PZM for usage.

=head1 DESCRIPTION

Tst provides a base implementation for the lifecycle
methods of a test suite.

=head1 METHODS

=cut

=head2 info()

Base implementation of the info method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=item \%info

A hash ref to the INFO section from the test suite.

=back

=head3 Return

Standard JSON reply.

=cut
sub info {
    my %ans=();       # anser hash, empry by default
    my $keyfound=0;   # FLAG : did we found a valid result?
    #
    my $data;
    my $datakey;
    my $tmp;
    my $err = 0;
    my $quiet=0;      # FLAG : do we need to run quiet?

    my @cmd = split(/:/, $_[0]);
    my (%info) = %{$_[1]};

    my $opt = getActionOpt(@cmd);
    if ($opt eq "q")
    {
        $quiet=1;
    }

    ### Get the Command to perform on this action, if any.
    if (defined $cmd[1])
    {
        my $infokey;
        my $lcmd = lc($cmd[1]);
        if ($lcmd eq "all")
        { 
            $keyfound=1;
            $ans{STATUS}="0";
            $ans{VALUE}=\%info;
        }
        elsif ($lcmd eq "list")
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
                if ($lcmd eq lc($infokey)) {
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

        if (!defined $info{INPUT}{PATH}) {
            $err+=1;
            $ans{VALUE}{ERROR}{$err}=
                "New() missing INPUT.PATH value in info: ".
                "FAILED.";
        }

        #
        # check CONFIG->$KEY->FILE vector syntax
        #
        $data = \%info;
        if (exists $info{CONFIG}{DATA}){
            for $datakey ( keys %{$data->{CONFIG}{DATA}} ) {
                if (exists $info{CONFIG}{DATA}{$datakey}{FILE}) {
                    # retreive the array of file to check
                    $tmp = $info{CONFIG}{DATA}{$datakey}{FILE};
                    $tmp = reftype $tmp;
                    # the FILE section must be an array, 
                    #even if with a single vallue
                    if (!($tmp =~ /^ARRAY/ )){
                        $err+=1;
                        $ans{VALUE}{ERROR}{$err}=
                            "New() syntax error in CONFIG : ".
                            "$tmp [$info{CONFIG}{DATA}{$datakey}{FILE}] : ".
                            "FAILED.";
                    }
                }
                else{
                    $err+=1;
                    $ans{VALUE}{ERROR}{$err}=
                        "New() missing DATA->FILE value in CONFIG [$tmp] : ".
                        "FAILED.";
                }
            }
        }
        #
        # check INPUT->$KEY->FILE vector syntax
        #
        if(exists $info{INPUT}{DATA}){
            for $datakey ( keys %{$data->{INPUT}{DATA}} ) {
                if (exists $info{INPUT}{DATA}{$datakey}{FILE}) {
                    # retreive the array of file to check
                    $tmp = $info{INPUT}{DATA}{$datakey}{FILE};
                    $tmp = reftype $tmp;
                    # the FILE section must be an array, 
                    # even if with a single vallue
                    if (!($tmp =~ /^ARRAY/ )){
                        $err+=1;
                        $ans{VALUE}{ERROR}{$err}=
                            "New() syntax error in INPUT : ".
                            "$tmp [$info{INPUT}{DATA}{$datakey}{FILE}] : ".
                            "FAILED.";
                    }
                }
                else{
                    $err+=1;
                    $ans{VALUE}{ERROR}{$err}=
                        "New() missing DATA->FILE value in OUTPUT [$tmp] : ".
                        "FAILED.";
                }
            }
        }
        #
        # check OUTPUT->$KEY->FILE vector syntax
        #
        # FIXIT refactor same pattern as above and above, util methods
        if (exists $info{OUTPUT}{DATA}){
            for $datakey ( keys %{$data->{OUTPUT}{DATA}} ) {
                if (exists $info{OUTPUT}{DATA}{$datakey}{FILE}) {
                    # retreive the array of file to check
                    $tmp = $info{OUTPUT}{DATA}{$datakey}{FILE};
                    $tmp = reftype $tmp;
                    # the FILE section must be an array, 
                    # even if with a single vallue
                    if (!($tmp =~ /^ARRAY/ )){
                        $err+=1;
                        $ans{VALUE}{ERROR}{$err}=
                            "New() syntax error in OUTPUT : ".
                            "$tmp [$info{OUTPUT}{DATA}{$datakey}{FILE}] : ".
                            "FAILED.";
                    }
                }
                else{
                    $err+=1;
                    $ans{VALUE}{ERROR}{$err}=
                        "New() missing DATA->FILE value in OUTPUT [$tmp] : ".
                        "FAILED.";
                }
            }
        }

        #
        # more syntax check t.b.d
        #
        # ....
        # ....
        # FIXIT need syntax check aka validator like regression!!! FIXIT FIXIT!
    }

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

=head2 new()

Base implementation for the new method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=item $Bin

The location of perl execution context.

=item \%info

A hash ref to the INFO section from the test suite.

=item \%env

A hash ref to the ENV section from the test suite.

=item \&callback()

A callback to fetch files after wget; takes 2 args:

=over 4

=item $root

The filesystem root to the test suite.

=item \%INFO

A hash ref to the INFO section from the test suite.

=back

=back

=head3 Return

Standard JSON reply.

=cut
sub new{
    my @cmd;          # command input string
    my %ans=();       # anser hash, empry by default
    my $tptselect;
    my $jobselect;
    my $cmd="";
    #
    my $workflow={};
    my $worklist={};
    my $infoCmd;
    my $info;
    my @descriptor;
    my $tmp;
    my $work;
    my $logMsg;
    my $quiet=0;      # FLAG : do we need to run
    #
    my $tstabspath;
    my $jobabspath;
    
    my $err = 0;
    #

    @cmd = split(/:/,$_[0]);
    my $Bin = $_[1];
    my (%INFO) = %{$_[2]};
    my (%ENV) = %{$_[3]};
    my $callback = $_[4];
    
    ### Get the option for the CMD, if any.
    $cmd = getActionCmd(@cmd);

    ### Get the option for the CMD, if any.
    my $opt = getActionOpt(@cmd);
    if ($opt eq "q"){
        $quiet=1;
    }

    # single selection requested?
    if( $cmd=~/^job_/){
        $jobselect = $cmd;
    }
    #
    if( $cmd=~/^tpt_/){
        $tptselect = $cmd;
    }
    #
    $tptselect = $opt if ($cmd=~/^tpt_/);
    #
    if(lc($cmd) eq "all"){
        undef $tptselect;
        undef $jobselect;
    }


    # prepare foldernames
    $tstabspath = $Bin."/".$ENV{TSTRELPATH}."/";
    $jobabspath = $Bin."/".$ENV{JOBRELPATH}."/";


    # calling sanity check
    #
    if(lc($INFO{TYPE}) eq "virtual"){
        # virtual TST should never exists
        $err+=1;
        $ans{VALUE}{ERROR}{$err}="virtual TST, cannot call New.";
    }
    else{
        # read the info of this TST
        # we read calling the function to avoid
        # syntax check twice
        # get the pointer to the data struct.
        my $info = info("info:all:q", \%INFO  );
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
                            $logMsg = "Name   : ".$ENV{NAME}."\n"."Title  : ".
                                $ENV{TITLE}."\n"."Detail : ".$ENV{DETAIL}."\n";
                            logFileDump($tmp,$logMsg);
                            logFileDump($tmp,"\n#=========================".
                                        "==============================\n");
                            my $jobj = 
                                JSON->new->utf8(1)->pretty(1)->encode(\%ENV);
                            $jobj=lc($jobj); logFileDump($tmp,$jobj);
                            logFileDump($tmp,"\n#========================".
                                        "===============================\n");
                            close(LOGFILE);
                            $logMsg = "New , CREATE , ".getDateTimeNow();
                            logFileDump($tmp,$logMsg);
                        }
                        else{
                            $err+=1;
                            $ans{VALUE}{ERROR}{$err}=
                                "New() previous TST not finalized : FAILED.";
                        }
                    }
                    else{
                        # no previous passes in the logFile
                        $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
                        open(LOGFILE, ">$tmp");
                        $logMsg = "Name   : ".$ENV{NAME}."\n"."Title  : ".
                            $ENV{TITLE}."\n"."Detail : ".$ENV{DETAIL}."\n";
                        logFileDump($tmp,$logMsg);
                        logFileDump($tmp,"\n#============================".
                                    "===========================\n");
                        my $jobj = JSON->new->utf8(1)->pretty(1)->encode(\%ENV);
                        $jobj=lc($jobj); logFileDump($tmp,$jobj);
                        logFileDump($tmp,"\n#=============================".
                                    "==========================\n");
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
                    $ans{VALUE}{ERROR}{$err}=
                        "New() unable to create sandbox dir : FAILED.";
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
                    #
                    # check for the /bin/exebin files, 
                    # if not there do a wget and check again.
                    #
                    $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
                    $logMsg = "New , CHECK_EXEBIN(1), ".getDateTimeNow();
                    logFileDump($tmp,$logMsg);
                    # check CONFIG->$KEY->FILE vector syntax
                    #
                    my $datakey;
                    my $data = \%INFO;
                    #
                    if (exists $INFO{EXEBIN}{DATA}){
                        for $datakey ( keys %{$data->{EXEBIN}{DATA}} ) {
                            if (exists $INFO{EXEBIN}{DATA}{$datakey}{CMD}) {
                                # retreive the array of file to check
                                $tmp = $tstabspath."../bin/".
                                    $INFO{EXEBIN}{DATA}{$datakey}{CMD};
                                if (!(-f $tmp)){
                                    $err+=1;
                                    # NOTE: I do not add an error message 
                                    # since I'm gonna erase
                                    # this error condition and check again.
                                }
                            }
                        }
                    }
                    #
                    # check: did we had an error? do a wget and re-check
                    # NOTE : this looks tricky but it's correct since we enter
                    #        previous section ONLY whith err==0.
                    if($err){
                        # update logFile
                        $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
                        $logMsg = "New , WGET, ".getDateTimeNow();
                        logFileDump($tmp,$logMsg);
                        #
                        # get the binary from the server
                        $tmp = "cd ". $tstabspath."../bin/ && ";
                        $tmp = $tmp.$INFO{SYSBIN}{WGET}{CMD}." -r -nH ftp://";
                        $tmp = $tmp.$INFO{EXEBIN}{REPO}{SERVER}."/".
                            $INFO{EXEBIN}{REPO}{PATH}."/* > /dev/null 2>&1";
                        system($tmp);
                        # copy the files: this is prj dependent, 
                        # i.e. hardcoded (we pbly need overloading)
                        $callback->($tstabspath, \%INFO);
                        # erase the err condition
                        #
                        $err=0;
                        #
                    }
                    #
                    # check AGAIN, do we have all needed exebin?
                    #
                    $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
                    $logMsg = "New , CHECK_EXEBIN(2), ".getDateTimeNow();
                    logFileDump($tmp,$logMsg);
                    #
                    if (exists $INFO{EXEBIN}{DATA}){
                        for $datakey ( keys %{$data->{EXEBIN}{DATA}} ) {
                            if (exists $INFO{EXEBIN}{DATA}{$datakey}{CMD}) {
                                # retreive the array of file to check
                                $tmp = $tstabspath."../bin/".
                                    $INFO{EXEBIN}{DATA}{$datakey}{CMD};
                                if (!(-f $tmp)){
                                    $err+=1;
                                    # NOTE: here I MUST add the error message!!
                                    $ans{VALUE}{ERROR}{$err}=
                                        "TST.New() : can't retrieve the ".
                                        "EXEBIN [$tmp]";
                                }
                                else{
                                    #exebin permissions
                                    chmod 0754, $tmp;
                                }
                            }
                        }
                    }

                    # update logFile
                    $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
                    $logMsg = "New , START, ".getDateTimeNow();
                    logFileDump($tmp,$logMsg);

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


        ## Tst.New() : CORE SECTION
        ## This is the part that have to be manually customized to build the
        ## right workflow of the tst, based on job specific definition.
        ##   <- for each virtual job in the worklist create real job file
        ##    -> build workflow.
        ##   <- for each real job in the workflow call JOB.New()
        ##
        ##
        ## NOTE: job are unique. for each job subfolder there is always one
        ##       and only one real job.
        if(!($err)){
            # simple counter for multiple TPT file naming.
            my $count=0;

            if (defined $jobselect){
                $err+=1; $ans{VALUE}{ERROR}{$err}=
                    "New() : JOB SELECT not yet implemented";
            }
            elsif(defined $tptselect){
                $err+=1; $ans{VALUE}{ERROR}{$err}=
                    "New() : TPT SELECT not yet implemented";
            }
            else{
                ## DEFAULT : Call New for all jobs

                # get the worklist of this job
                # get the pointer to the data struct.
                my $worklist = worklist( "worklist:all:q", $Bin );
                # sanity check
                if (defined $worklist->{STATUS}){
                    if(lc($worklist->{STATUS}) eq "0"){

                        # FIXIT not sure closure is so nice here...
                        for $work ( sort { 
                            $worklist->{VALUE}{$a}{PRIORITY} cmp 
                                $worklist->{VALUE}{$b}{PRIORITY} } 
                                    keys %{$worklist->{VALUE}}){

                            # get the info of this TST
                            $infoCmd = $jobabspath."/job_".$work."/job_".
                                $work.".pl -a info";

                            # get the info on each virtual tpt of the worklist
                            @descriptor = `$infoCmd`;
                            # check the status on $tmp[0]
                            my $string = join('',@descriptor); chomp($string);
                            $tmp = from_json($string);

                            my $realJobName = "job_".$work."_".$count.".pl"; 
                            $count+=1;

                            if(exists $tmp->{STATUS}){
                                if(lc($tmp->{STATUS}) eq (lc("0"))){

                                    my $ret = 
                                        setConfigJOB( $jobabspath."/job_".
                                                      $work."/job_".$work.
                                                      ".pl", 
                                                      $tmp->{VALUE}, 
                                                      $jobabspath."/job_".
                                                      $work."/".$realJobName, 
                                                      \%INFO, \%ENV );
                                    if($ret ne "0"){
                                        $err+=1; 
                                        $ans{VALUE}{ERROR}{$err}=
                                            "New() : job setConfigJob FAILED ".
                                            "[$work]";}
                                }
                                else{
                                    $err+=1; 
                                    $ans{VALUE}{ERROR}{$err}=
                                        "New() : job call failed [$work]";}
                            }
                            else{
                                $err+=1; 
                                $ans{VALUE}{ERROR}{$err}=
                                    "New() : job call undefined [$work]";}
                        }# JOB loop
                    }
                    else{
                        $err+=1; 
                        $ans{VALUE}{ERROR}{$err}="New() : job worklist error";
                    }
                }
                else{
                    $err+=1; 
                    $ans{VALUE}{ERROR}{$err}="New() : job worklist undefined";
                }

                ##
                ## CORE SECTION:CALL TPT.New() for each real TPT in the workflow
                ##
                if (!($err)){
                    # get the worklist of this job
                    my $workflow = workflow( "workflow:all:q", $Bin);

                    # sanity check
                    if (defined $workflow->{STATUS})
                    {
                        if(lc($workflow->{STATUS}) eq "0"){
                            for $work ( sort 
                                        { $workflow->{VALUE}{$a}{PRIORITY} 
                                          cmp $workflow->{VALUE}{$b}{PRIORITY}}
                                        keys %{$workflow->{VALUE}} ){
                                # call the New() of this JOB
                                $infoCmd = $jobabspath."/job_".$work."/".
                                    $workflow->{VALUE}{$work}{FILE}." -a new";

                                @descriptor = `$infoCmd`;
                                # check the status on $tmp[0]
                                my $string = join('',@descriptor); 
                                chomp($string);
                                $tmp = from_json($string);
                                if(exists $tmp->{STATUS}){
                                    if(lc($tmp->{STATUS}) ne (lc("0"))){
                                        $err+=1;
                                        $ans{VALUE}{ERROR}{$err}=
                                            "New() : Job.New() error on ".
                                            "[$work] : FAILED.";
                                    }
                                }
                                else{ 
                                    $err+=1; 
                                    $ans{VALUE}{ERROR}{$err}=
                                        "New() : Job.New() on [$work] : ".
                                        "UNDEFINED.";}
                            }
                        }
                        else{ 
                            $err+=1; 
                            $ans{VALUE}{ERROR}{$err}="New() : Workflow ".
                                "request : FAILED.";}
                    }
                    else{ 
                        $err+=1; 
                        $ans{VALUE}{ERROR}{$err}="New() : Workflow request : ".
                            "UNDEFINED.";}
                }
            }
        }#if not err
        ##
        ## CHECK : POST section
        ##         we loop check on configs / input / output to match
        ##         test conditions from the $INFO section.
        ##
        if (!($err)){
            # t.b.d :
            # verify the result of each TPT.new call output
        }
    }# // IF(VIRTUAL)


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
    my $taskdir = $Bin.'/./';
    my @tasklist;
    my $jobdir;
    my $job;
    my @filelist;
    my @joblist;
    my @descriptor;
    my %tasks;
    my $tmp;
    my $err = 0;

    ### Get the option for the CMD, if any.
    my $opt = getActionOpt(@cmd);
    if ($opt eq "q")
    {
        $quiet=1;
    }

    opendir(IMD, $taskdir) || die("Cannot open directory");
    @filelist = readdir(IMD);
    closedir(IMD);

    #filter the filenames
    foreach $task (@filelist){
        if ( -d $taskdir."/".$task){
            if ( $task =~ /^job_(\w*)/i ){

                $jobdir = $Bin."/".$task;
                opendir(IMD, $jobdir) || die("Cannot open directory");
                @joblist = readdir(IMD);
                closedir(IMD);

                foreach $job (@joblist){
                    $tmp = $Bin."/".$task."/".$job;

                    if( -f $tmp){
                        push(@tasklist, $task);
                        # get the priority
                        @descriptor = `$Bin/$task/$job  -a info`;
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
                                        $tasks{$1}{PRIORITY} = 
                                            $tmp->{VALUE}{PRIORITY};
                                        #add the filename of the job
                                        $tasks{$1}{FILE}=$job;
                                    }
                                }
                                else{ 
                                    $err+=1; 
                                    $ans{VALUE}{ERROR}{$err}="worklist : ".
                                        "undefined priority for [$task]";
                                }
                            }
                            else{ 
                                $err+=1;
                                $ans{VALUE}{ERROR}{$err}="worklist : ".
                                    "error on JOB info request [$task]";
                            }
                        }
                        else{ 
                            $err+=1; 
                            $ans{VALUE}{ERROR}{$err}="worklist : ".
                                "bad JOB found [$task]";
                        }
                    }
                }
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

    my $Bin = $_[1];

    my @cmd;          # command input string
    my %ans=();       # anser hash, empry by default
    my $quiet=0;      # FLAG : do we need to run quiet?
    my $keyfound=0;   # FLAG : did we found a valid result?
    #
    my $task;
    my $taskdir = $Bin.'/./';
    my @tasklist;
    my @filelist;
    my @descriptor;
    my %tasks;
    my $tmp;
    my $err = 0;

    @cmd = split(/:/,$_[0]);

    ### Get the option for the CMD, if any.
    my $opt = getActionOpt(@cmd);
    if ($opt eq "q")
    {
        $quiet=1;
    }

    opendir(IMD, $taskdir) || die("Cannot open directory");
    @filelist = readdir(IMD);
    closedir(IMD);

    #filter the filenames
    foreach $task (@filelist){
        if ( $task =~ /^job_(\w*)/i ){
            $tmp = $Bin."/".$task."/".$task.".pl";
            if( -f $tmp)
            {
                push(@tasklist, $task);
                # get the priority
                @descriptor = `$Bin/$task/$task.pl  -a info`;
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
                            $ans{VALUE}{ERROR}{$err}="worklist : ".
                                "undefined priority for [$task]";}
                    }
                    else{ 
                        $err+=1;
                        $ans{VALUE}{ERROR}{$err}="worklist : ".
                            "error on JOB info request [$task]";}
                }
                else{ $err+=1; $ans{VALUE}{ERROR}{$err}="worklist : bad ".
                          "JOB found [$task]";}
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

=head2 run()

Base implementation of the run method; simply runs each job.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=item $Bin

The location of perl execution context.

=item \%INFO

A hash ref to the INFO section from the test suite.

=item \%ENV

A hash ref to the ENV section from the test suite.

=back

=head3 Return

Standard JSON reply.

=cut
sub run{

    my @cmd;          # command input string
    my %ans=();       # anser hash, empry by default
    my $quiet=0;      # FLAG : do we need to run quiet?
    my $tptselect;
    my $jobselect;
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

    @cmd = split(/:/,$_[0]);
    my $Bin = $_[1];
    my (%INFO) = %{$_[2]};
    my (%ENV) = %{$_[3]};

    ### Get the option for the CMD, if any.
    $cmd = getActionCmd(@cmd);

    my $opt = getActionOpt(@cmd);
    if ($opt eq "q"){
        $quiet=1;
    }

    # single selection requested?
    if($cmd=~/^job_/){
        $jobselect = $cmd;
    }
    #
    if($cmd=~/^tpt_/){
        $tptselect = $cmd;
    }
    #
    $tptselect = $opt if ($cmd=~/^tpt_/);
    #
    if(lc($cmd) eq "all"){
        undef $tptselect;
        undef $jobselect;
    }

    # prepare foldernames
    $tstabspath = $Bin."/".$ENV{TSTRELPATH}."/";
    $jobabspath = $Bin."/".$ENV{JOBRELPATH}."/";


    # calling sanity check
    #
    if(lc($INFO{TYPE}) eq "virtual"){
        $err+=1;
        $ans{VALUE}{ERROR}{$err}="virtual TST, cannot call Run.";
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
            $ans{VALUE}{ERROR}{$err}="Run : TST not intitialized correctly.";
        }
        else{
            ##
            ## CHECK : PRE section
            ##         we loop check on configs / input / output to match
            ##         test conditions from the $INFO section.
            ##

            ##
            ## CORE SECTION : CALL JOB.Run() for each real JOB in the workflow
            ## The order is very important.
            ##
            if (!($err)){
                # get the worklist of this job
                $workflow = workflow( "workflow:all:q", $Bin );

                # sanity check
                if (defined $workflow->{STATUS}){
                    if(lc($workflow->{STATUS}) eq "0"){

                        if (defined $jobselect){
                            ## asking to run a specific JOB??
                            $err+=1; 
                            $ans{VALUE}{ERROR}{$err}=
                                "New() : JOB SELECT not yet implemented";
                        }
                        elsif(defined $tptselect){
                            ## asking to run a specific TestPoint??
                            $err+=1; 
                            $ans{VALUE}{ERROR}{$err}=
                                "New() : TPT SELECT not yet implemented";
                        }
                        else{
                            ## no request for specific run
                            ## running all the jobs in priority order.
                            for $work 
                                ( sort { $workflow->{VALUE}{$a}{PRIORITY} cmp 
                                             $workflow->{VALUE}{$b}{PRIORITY}}
                                  keys %{$workflow->{VALUE}} ){
                                    # call the New() of this TST
                                    $infoCmd = $jobabspath."/job_".$work."/".
                                        $workflow->{VALUE}{$work}{FILE}.
                                        " -a run";
                                    @descriptor = `$infoCmd`;
                                    # check the status on $tmp[0]
                                    my $string = join('',@descriptor); 
                                    chomp($string);
                                    $tmp = from_json($string);
                                    if(exists $tmp->{STATUS}){
                                        if(lc($tmp->{STATUS}) ne (lc("0"))){
                                            $err+=1;
                                            $ans{VALUE}{ERROR}{$err}=
                                                "Tst.Run() : got a Job.RUN()".
                                                " error on [$work] : FAILED.";
                                        }
                                    }
                                    else{ 
                                        $err+=1; 
                                        $ans{VALUE}{ERROR}{$err}="Tst.Run() ".
                                            ": got a Job.Run() on [$work] : ".
                                            "UNDEFINED.";}
                            }# //for
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

    JSONLog("jTXT",\%ans) if (!$quiet);
    return \%ans;
}

=head2 new()

Base implementation for the finalize method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=item $Bin

The location of perl execution context.

=item \%info

A hash ref to the INFO section from the test suite.

=item \%env

A hash ref to the ENV section from the test suite.

=item \&callback()

A callback to generate tstView results output.

=over 4

=item $tstabspath

The absolute path to the root of the tst.

=item $jobabspath

The absolute path to the root of the job FIXIT which one?

=item \%INFO

A hash ref to the INFO section from the test suite.

=item \%ENV

A hash ref to the ENV section from the test suite.

=back

=back

=head3 Return

Standard JSON reply.

=cut
sub finalize{
    my @cmd;          # command input string
    my %ans=();       # anser hash, empry by default
    my $opt="";       # input option for the command
    my $quiet=0;      # FLAG : do we need to run quiet?
    my $tptselect;
    my $jobselect;
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

    @cmd = split(/:/, $_[0]);
    my $Bin = $_[1];
    my (%INFO) = %{$_[2]};
    my (%ENV) = %{$_[3]};
    my $cleanRaws = $_[4];
    my $callback = $_[5];

    ### Get the option for the CMD, if any.
    $cmd = getActionCmd(@cmd);

    ### Get the option for the CMD, if any.
    $opt = getActionOpt(@cmd);
    if ($opt eq "q")
    {$quiet=1;}

    # single selection requested?
    if( ($cmd=~/^job_/) && (!($quiet))){
        $jobselect = $cmd;
    }
    #
    if( ($cmd=~/^tpt_/) && (!($quiet))){
        $tptselect = $cmd;
    }
    #
    $tptselect = $opt if ($cmd=~/^tpt_/);
    #
    if(lc($cmd) eq "all"){
        undef $tptselect;
        undef $jobselect;
    }

    # prepare foldernames
    $tstabspath = $Bin."/".$ENV{TSTRELPATH}."/";
    $jobabspath = $Bin."/".$ENV{JOBRELPATH}."/";


    # sanity check
    if (lc($INFO{TYPE}) eq "virtual"){
        # virtual TST cannot control input
        $err+=1;
        $ans{VALUE}{ERROR}{$err}="virtual TST, cannot Finalize";
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
                $ans{VALUE}{ERROR}{$err}=
                    "Finalize : JOB did not Run correctly.";
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
                    # check for previous New() call result
                    #
                    $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
                    if(!(logFileParse($tmp,"Run , FINISH"))){
                        $err+=1;
                        $ans{VALUE}{ERROR}{$err}=
                            "Run : TST did not Run correctly.";
                    }
                }

                ##
                ## Finalize : Core Code Section
                ##
                if (!($err)){
                    # get the worklist of this job
                    $workflow = workflow("workflow:all:q", $Bin );

                    # sanity check
                    if (defined $workflow->{STATUS}){
                        if(lc($workflow->{STATUS}) eq "0"){

                            if (defined $jobselect){
                                ## asking to run a specific JOB??
                                $err+=1; 
                                $ans{VALUE}{ERROR}{$err}= "Finalize() : ".
                                    "JOB SELECT not yet implemented";
                            }
                            elsif(defined $tptselect){
                                ## asking to run a specific TestPoint??
                                $err+=1; 
                                $ans{VALUE}{ERROR}{$err}="Finalize() : ".
                                    "TPT SELECT not yet implemented";
                            }
                            else{
                                ## no request for specific run
                                ## running all the jobs in priority order.
                                for $work ( sort { $workflow->
                                                   {VALUE}{$a}{PRIORITY} 
                                                   cmp $workflow->
                                                   {VALUE}{$b}{PRIORITY} } 
                                            keys %{$workflow->{VALUE}} ){
                                    # call the New() of this TST
                                    $infoCmd = $jobabspath."/job_".$work."/".
                                        $workflow->{VALUE}{$work}{FILE}.
                                        " -a finalize";
                                    @descriptor = `$infoCmd`;
                                    # check the status on $tmp[0]
                                    my $string = join('',@descriptor); 
                                    chomp($string);
                                    $tmp = from_json($string);
                                    if(exists $tmp->{STATUS}){
                                        if(lc($tmp->{STATUS}) ne (lc("0"))){
                                            $err+=1;
                                            $ans{VALUE}{ERROR}{$err}="New() : ".
                                                "Job.Finalize() error on ".
                                                "[$work] : FAILED.";
                                        }
                                    }
                                    else{ 
                                        $err+=1; 
                                        $ans{VALUE}{ERROR}{$err}="New() : ".
                                            "Job.Finalize() on [$work] : ".
                                            "UNDEFINED.";}
                                }# //for
                            }
                        }
                        else{ 
                            $err+=1; 
                            $ans{VALUE}{ERROR}{$err}=
                                "Workflow request : FAILED.";
                        }
                    }
                    else{ 
                        $err+=1; 
                        $ans{VALUE}{ERROR}{$err}=
                            "Workflow request : UNDEFINED.";
                    }
                }#// if (!err)
            }#//else

            ##
            ## CHECK : POST section
            ##         we loop check on configs / input / output to match
            ##         test conditions from the $INFO section.
            ##
            if(!($err)){
                ##
                ## create html tstView
                my $gen = $callback->($tstabspath, $jobabspath, \%INFO, \%ENV);
                if ($gen != 0) {
                    $err += 1;
                    $ans{VALUE}{ERROR}{$err} = $gen;
                }
                    
                ##
                ## remove  all the raw files
                if ($cleanRaws) {
                    $cmd = "find ".$ENV{OUTPUT}{PATH}.
                        " -name '*.raw' -exec \\rm {} \\;";
                    system($cmd);
                }

                ##
                ## allow the rigth permissions on the OUTPUT for
                ## webserver presentation
                $cmd = "find ".$ENV{OUTPUT}{PATH}.
                    " -name '*' -exec chmod 755 {} \\;";
                system($cmd);

                ##
                ## allow the rigth permissions on the TSTVIEW for 
                ## webserver presentation
                $cmd = "find ".$ENV{OUTPUT}{PATH}.
                    "../tstView -name '*' -exec chmod 755 {} \\;";
                system($cmd);


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
        $ans{VALUE}{ERROR}{$err}="TST.Finalized : FAILED.";
        # LogFile update
        $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
        $logMsg = "Finalize , FAILED , ".getDateTimeNow();
        logFileDump($tmp,$logMsg);
    }

    # remove sandbox no matter what.
    $tmp = $ENV{SANDBOX}{PATH};
    if(-d $tmp){
        $tmp = $ENV{SYSBIN}{RM}{CMD}." -rf ".$ENV{SANDBOX}{PATH};
        system($tmp);
    }

    # return 
    JSONLog("jTXT",\%ans) if (!$quiet);
    # always return an hash
    return \%ans;
}

#
# SUBROUTINE :
# {{{ setConfigJOB Function

# Purpose: create a configuration file for JOB
#          based on job $INFO struct
#
# Params : $_[0] virtual job name
#          $_[1] job info
#          $_[2] real job name
#
sub setConfigJOB{
    my $ret = 0;
    my @joblines;
    my @jobPreHeader;
    my @jobPostHeader;
    #
    my $line;

    # NOTE: by default nothing can be put at the begining of JOB/TPT files other than
    #       ENV and INFO section, so we set the flag to 1 by default
    my $flagPre=1;
    my $flagPost=0;
    my $flag=0;

    if( (defined $_[0]) && (defined $_[1]) && (defined $_[2]) ){
        ## build the new ENV section based on the input INFO section
        ##
        my (%INFO) = %{$_[3]};
        my (%ENV) = %{$_[4]};

        my %JOBENV;
        my $key;
        $JOBENV{TYPE}="real";
        $_[1]->{TYPE}="real";
        $JOBENV{NAME}=$_[1]->{NAME};
        $JOBENV{TITLE}=$_[1]->{TITLE};
        $JOBENV{DETAIL}=$_[1]->{DETAIL};
        $JOBENV{TSTRELPATH}="../";
        $JOBENV{JOBRELPATH}="./";
        # EXEBIN ENV section
        for $key ( keys %{$_[1]->{EXEBIN}{DATA}} ){
            $JOBENV{EXEBIN}{$key}{CMD} = $_[1]->{EXEBIN}{DATA}{$key}{CMD};
        }
        # SYSBIN ENV section
        for $key ( keys %{$_[1]->{SYSBIN}} ){
            $JOBENV{SYSBIN}{$key}{CMD} = $_[1]->{SYSBIN}{$key}{CMD};
            $JOBENV{SYSBIN}{$key}{PATH} = ""; #path discovery to be added
        }
        # CONFIG ENV section
        $JOBENV{CONFIG}{PATH} = $ENV{CONFIG}{PATH}."/".$_[1]->{CONFIG}{DIR};
        for $key ( keys %{$_[1]->{CONFIG}{DATA}} ){
            $JOBENV{CONFIG}{DATA}{$key}{PATH} = $JOBENV{CONFIG}{PATH}."/".
                $_[1]->{CONFIG}{DATA}{$key}{DIR};
            $JOBENV{CONFIG}{DATA}{$key}{FILE} =
                $_[1]->{CONFIG}{DATA}{$key}{FILE};
        }
        # INPUT ENV section
        $JOBENV{INPUT}{PATH} = $ENV{INPUT}{PATH}."/".$_[1]->{INPUT}{DIR};
        for $key ( keys %{$_[1]->{INPUT}{DATA}} ){
            $JOBENV{INPUT}{DATA}{$key}{PATH} = $ENV{INPUT}{PATH}."/".
                $_[1]->{INPUT}{DIR}."/".
                $_[1]->{INPUT}{DATA}{$key}{DIR};
            $JOBENV{INPUT}{DATA}{$key}{FILE} = 
                $_[1]->{INPUT}{DATA}{$key}{FILE};
        }

        # OUTPUT ENV section
        $JOBENV{OUTPUT}{PATH} = $ENV{OUTPUT}{PATH}."/".$_[1]->{OUTPUT}{DIR};
        for $key ( keys %{$_[1]->{OUTPUT}{DATA}} ){
            $JOBENV{OUTPUT}{DATA}{$key}{PATH} = $ENV{OUTPUT}{PATH}."/".
                $_[1]->{OUTPUT}{DIR};
            $JOBENV{OUTPUT}{DATA}{$key}{FILE} = 
                $_[1]->{OUTPUT}{DATA}{$key}{FILE};
        }
        # SANDBOX ENV section
        $JOBENV{SANDBOX}{PATH}= $ENV{OUTPUT}{PATH}."/".$_[1]->{OUTPUT}{DIR}.
            "/".$_[1]->{SANDBOX}{DIR};

        # HWSELECT
        $JOBENV{HWSELECT} = $_[1]->{HWSELECT};

        ## read the template TPT file
        ##
        open MYTPT, "<", $_[0];
        @joblines = <MYTPT>;
        close MYTPT;

        ## parse the file and remove the ENV and INFO section.
        ##
        for $line (@joblines){
            #
            # note I don;t do any sanity check here
            #      on INFO and ENV unique count
            #      t.b.d : making sure we have only one ENV and one INFO
            #
            if(($line =~ /HEADER/)&&($line=~/BEGIN/)) {$flag = 1; $flagPre= 0;};
            if(($line =~ /HEADER/)&&($line=~/END/)) {$flag = 0; $flagPost= 1;};

            #print $flag."[".$line."\n";#$line."\n" if($flag);
            push(@jobPreHeader,  $line) if($flagPre);
            push(@jobPostHeader, $line) if($flagPost);

        }

        ## Create the TPT file
        ##
        # dump the pre header section
        #
        open MYTPT, ">", $_[2];
        $line =join("",@jobPreHeader);
        print MYTPT $line;

        # now dump the %ENV section
        #
        $Data::Dumper::Useqq  = 1;
        $line = Data::Dumper->Dump([ \%JOBENV ], [ '%ENV' ]);
        $line = substr $line, 1; # dumper fix   : remove first char
        $line = "my ".$line;     # dumper trick : add the declarator
        $line =~ s/{/(/;       # dumper fix for "\{"
        $line =~ s/};/);/;       # dumper fix for "};"
        print MYTPT $line;

        # now dump the %INFO section
        #
        $Data::Dumper::Useqq  = 1;
        $line = Data::Dumper->Dump([ \$_[1] ], [ '%INFO' ]);
        $line = substr $line, 1; # dumper fix   : remove first char
        $line = "my ".$line;     # dumper trick : add the declarator
        $line =~ s/\\{/(/;       # dumper fix for "\{"
        $line =~ s/};/);/;       # dumper fix for "};"
        print MYTPT $line;

        ## append the rest of the TPT body
        ##
        $line =join("",@jobPostHeader);
        print MYTPT $line;
        close MYTPT;

        # set the permissions
        chmod 0754, $_[2];
    }
    else{
        $ret = -1;
    }
    ##
    return $ret;
}

=head1 COPYRIGHT

Copyright 2011 Gianluca Filippini, Kevin McIntire
All Rights Reserved

=cut


1;
