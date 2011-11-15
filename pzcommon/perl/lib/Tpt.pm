#
# Copyright 2010 Gianluca Filippini, Kevin McIntire
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
package Tpt;

use strict;
use warnings;
use Exporter;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
my $VERSION     = 1.00;
@ISA         = qw(Exporter);

use SysTool;
require SysCheck;
use JSONLog;
use FindBin qw($Bin);

=head1 NAME

Tpt - Base implementation for a test point (TPT) in Puzzle.

=head1 SYNOPSIS

Refer to any implemented perl PZM for usage.

=head1 DESCRIPTION

Tpt provides a base implementation for the lifecycle
methods of a tpt.

=head1 METHODS

=cut

=head2 info()

Base implementation of the info method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=item \%info

A hash ref to the INFO section from the test point.

=back

=head3 Return

Standard JSON reply.

=cut
sub info{
    my $err = 0;
    my %ans=();       # anser hash, empry by default
    my $quiet=0;      # FLAG : do we need to run quiet?
    my $keyfound=0;   # FLAG : did we found a valid result?

    my @cmd = split(/:/,$_[0]);
    my %INFO = %{$_[1]};

    ### Get the option for the CMD, if any.
    my $opt = getActionOpt(@cmd);
    if ($opt eq "q" ){
        $quiet=1;
    }

    ### Get the Command to perform on this action, if any.
    # FIXIT this is shared by pm in common, refactor
    if (defined $cmd[1])
    {
        my $infokey; 
        my $cm = lc($cmd[1]);
        if ($cm eq"all")
        { 
            $keyfound=1;
            $ans{STATUS}="0";
            $ans{VALUE}=\%INFO;
        }
        elsif ($cm eq "list")
        {
            $keyfound=1;
            $ans{STATUS}="0";
            for $infokey ( sort keys %INFO ){
                $ans{VALUE}{$infokey}="1";
            }
        }
        else {
            for $infokey ( sort keys %INFO ){
                if ($cm eq lc($infokey) ){
                    $keyfound=1;
                    $ans{STATUS}="0";
                    $ans{VALUE}{$infokey}=$INFO{$infokey};
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
        $ans{VALUE}=\%INFO;
    }

    # SANITY CHECK : obj. info sintax
    # based on the API/obj version spec.
    #
    if(!($err)){
        if(exists ($INFO{NAME})){
            if($INFO{NAME} =~ /[@!#\$%\^&\*:;"'?<>~`\s\r\n\t]/i){
                $err+=1;
                $ans{VALUE}{ERROR}{$err}=
                    "obj info error : incorrect name syntax [$INFO{NAME}]";
            }
        }
        else{
            $err+=1;
            $ans{VALUE}{ERROR}{$err}="obj sintax error : name undefined";
        }

        #
        # t.b.d. check here for MANDATORY field like priority, name, etc, etc
        #

        #
        # more syntax check t.b.d
        #
        # Check for fields required by Puzzle
        if (!defined $INFO{TSTRELPATH}) {
            $err+=1;
            $ans{VALUE}{ERROR}{$err}=
                "New() missing TSTRELPATH value in info: ".
                "FAILED.";
        }

        if (!defined $INFO{CLUSTER}{TYPE}) {
            $err+=1;
            $ans{VALUE}{ERROR}{$err}=
                "New() missing CLUSTER.TYPE value in info: ".
                "FAILED.";
        }

        if (!defined $INFO{OUTPUT}{DIR}) {
            $err+=1;
            $ans{VALUE}{ERROR}{$err}=
                "New() missing OUTPUT.DIR value in info: ".
                "FAILED.";
        }

        if (!defined $INFO{OS}{BRAND}) {
            $err+=1;
            $ans{VALUE}{ERROR}{$err}=
                "New() missing OS.BRAND value in info: ".
                "FAILED.";
        }

        if (!defined $INFO{OS}{BIT}) {
            $err+=1;
            $ans{VALUE}{ERROR}{$err}=
                "New() missing OS.BIT value in info: ".
                "FAILED.";
        }

        if (!defined $INFO{HW}{ISET}) {
            $err+=1;
            $ans{VALUE}{ERROR}{$err}=
                "New() missing HW.ISET value in info: ".
                "FAILED.";
        }

        if (!defined $INFO{HW}{ARCH}) {
            $err+=1;
            $ans{VALUE}{ERROR}{$err}=
                "New() missing HW.ARCH value in info: ".
                "FAILED.";
        }

        if (!defined $INFO{INPUT}{DATA}) {
            $err+=1;
            $ans{VALUE}{ERROR}{$err}=
                "New() missing INPUT.DATA value in info: ".
                "FAILED.";
        }

        if (!defined $INFO{INPUT}{DIR}) {
            $err+=1;
            $ans{VALUE}{ERROR}{$err}=
                "New() missing INPUT.DIR value in info: ".
                "FAILED.";
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
        $ans{VALUE}{ERROR}{$err}="error in action cmd exec.";
    }
    else{
        $ans{STATUS}="0";
    }
    JSONLog("jTXT",\%ans) if (!$quiet);
    # always return an hash
    return \%ans;
}
# }}}


=head2 workflow()

Base implementation of the workflow method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=back

=head3 Return

Standard JSON reply.

=cut
sub workflow{
    my %ans=();       # anser hash, empry by default
    my $quiet=0;      # FLAG : do we need to run quiet?
    my $err = 0;

    my @cmd = split(/:/,$_[0]);

    ### Get the option for the CMD, if any.
    my $opt = getActionOpt(@cmd);
    if ($opt eq "q"){
        $quiet=1;
    }

    # TPT does not have a workflow!!
    $err+=1;
    $ans{STATUS}="-1";
    $ans{VALUE}{ERROR}{$err}="TPTs does have only ONE workflow : TPT.Run";

    JSONLog("jTXT",\%ans) if (!$quiet);
    # always return an hash
    return \%ans;
}
# }}}


=head2 new()

Base implementation for the new method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=item $Bin

The location of perl execution context.

=item \%info

A hash ref to the INFO section from the test point.

=item \%env

A hash ref to the ENV section from the test point.

=back

=head3 Return

Standard JSON reply.

=cut
sub new{

    my %ans=();       # answer hash, empty by default
    my $quiet=0;      # FLAG : do we need to run quiet?
    #
    my %tmp;
    my $tmp;
    my $logMsg;
    #
    my $datakey;
    my $eventkey;
    #
    my $err = 0;

    my @cmd = split(/:/,$_[0]);
    my $Bin = $_[1];
    my %INFO = %{$_[2]};
    my %ENV = %{$_[3]};

    ### Get the option for the CMD, if any.
    my $opt = getActionOpt(@cmd);
    if ($opt eq "q"){
        $quiet=1;
    }

    # prepare foldernames
    my $tstabspath = $Bin."/".$ENV{TSTRELPATH}."/";
    my $jobabspath = $Bin."/".$ENV{JOBRELPATH}."/";

    # calling sanity check
    #
    if(lc($INFO{TYPE}) eq "virtual"){
        # virtual TPT cannot control input
        $err+=1;
        $ans{VALUE}{ERROR}{$err}="virtual TPT, cannot call New.";
    }
    else{
        # check the INFO->OS section
        #
        if(exists $INFO{OS}{CHECK}){
            $tmp{OS}=$INFO{OS};
            if (!(SysCheck::CheckOS(\%tmp))){
                $err+=1;
                $ans{VALUE}{ERROR}{$err}=
                    "HostOS does not match the TPT requested OS";
            }
        }
        # check the INFO->HW type
        #
        if(exists $INFO{HW}{CHECK}){
            $tmp{HW}=$INFO{HW};
            if (!(SysCheck::CheckHW(\%tmp))){
                $err+=1;
                $ans{VALUE}{ERROR}{$err}=
                    "HostHW does not match the TPT requested HW.";
            }
        }

        # read the info of this TPT
        # we read calling the function to avoid
        # syntax check twice
        my $info = info("info:all:q" , \%INFO );
        # sanity check
        if (defined $info->{STATUS})
        {
            if($info->{STATUS} ne "0" ){
                $err+=1;
                $ans{VALUE}{ERROR}{$err}="New() info parsing : FAILED.";
            }
            else{
                # create the TPT output folder if needed
                if (!(-d $ENV{OUTPUT}{PATH})){
                    mkdir $ENV{OUTPUT}{PATH};
                }
                # now double check permissions
                if (!(-d $ENV{OUTPUT}{PATH})){
                    $err+=1;
                    $ans{VALUE}{ERROR}{$err}=
                        "New() unable to create output dir : FAILED.[".
                        $ENV{OUTPUT}{PATH}."]";
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
                if(-f $tmp)
                {
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
                            $jobj=$jobj; logFileDump($tmp,$jobj);
                            logFileDump($tmp,"\n#===========================".
                                        "============================\n");
                            close(LOGFILE);
                            $logMsg = "New , CREATE , ".getDateTimeNow();
                            logFileDump($tmp,$logMsg);
                        }
                        else{
                            $err+=1;
                            $ans{VALUE}{ERROR}{$err}=
                                "New() previous TPT not finalized : FAILED.";
                        }
                    }
                    else{
                        # no previous passes in the logFile
                        $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
                        open(LOGFILE, ">$tmp");
                        $logMsg = "Name   : ".$ENV{NAME}."\n"."Title  : ".
                            $ENV{TITLE}."\n"."Detail : ".$ENV{DETAIL}."\n";
                        logFileDump($tmp,$logMsg);
                        logFileDump($tmp,"\n#==============================="
                                    ."========================\n");
                        my $jobj = JSON->new->utf8(1)->pretty(1)->encode(\%ENV);
                        $jobj=$jobj; logFileDump($tmp,$jobj);
                        logFileDump($tmp,"\n#===============================".
                                    "========================\n");
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
                    # FIXIT repetitive code below
                    # update logFile
                    $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
                    $logMsg = "New , START, ".getDateTimeNow();
                    logFileDump($tmp,$logMsg);
                    # loop over the $INFO{CONFIG}{KEYS} and check presence
                    # Note: the loop is performed on the $INFO section but the 
                    # check is done on the $ENV section because it's a real TPT,
                    # and a real TPT is supposed to have the $ENV correct.
                    #
                    for $datakey ( keys %{$info->{VALUE}{CONFIG}{DATA}} ) {
                        # check for presence requested??
                        if (exists 
                            $info->{VALUE}{CONFIG}{DATA}{$datakey}{CHECK}{NEW}{PRE}{PRESENCE}){
                            $tmp =  $ENV{CONFIG}{$datakey}{PATH}."/".
                                $ENV{CONFIG}{$datakey}{FILE};
                            if (!(-f $tmp)){
                                $err+=1;
                                $ans{VALUE}{ERROR}{$err}=
                                    "New() missing config [$tmp] : FAILED.";
                            }
                        }
                    }

                    # loop over the $INFO{INPUT}{KEYS}
                    for $datakey ( keys %{$info->{VALUE}{INPUT}{DATA}} ) {
                        # check for presence requested??
                        if (exists 
                            $info->{VALUE}{INPUT}{DATA}{$datakey}{CHECK}{NEW}{PRE}{PRESENCE}){
                            $tmp =  $ENV{INPUT}{$datakey}{PATH}."/".
                                $ENV{INPUT}{$datakey}{FILE};
                            if (!(-f $tmp)){
                                $err+=1;
                                $ans{VALUE}{ERROR}{$err}=
                                    "New() missing input [$tmp] : FAILED.";
                            }
                        }
                    }

                    # loop over $ENV{OUTPUT}{KEYS} and create needed output DIR
                    #
                    for $datakey ( keys %{$info->{VALUE}{OUTPUT}{DATA}} ) {
                        $tmp =  $ENV{OUTPUT}{$datakey}{PATH};
			if (defined $tmp) {
                        mkdir $tmp unless (-d $tmp);
                        if (!(-d $tmp)){
                            $err+=1;
                            $ans{VALUE}{ERROR}{$err}=
                                "New() unable to create output dir [$tmp] : ".
                                "FAILED.";
                        }
                        else{
                            chmod 0754, $tmp;
                        }
			}
                    }

                    # loop over the $ENV{EXEBIN}{KEYS} we ALWAYS need 
                    # to check their presence on New()
                    #
                    for $datakey ( keys %{$info->{VALUE}{EXEBIN}{DATA}} ) {
                        if(exists ($INFO{EXEBIN}{DATA}{$datakey}{CHECK})){
                            $tmp = 
                                $tstabspath."/bin/".$ENV{EXEBIN}{$datakey}{CMD};
                            if (!(-f $tmp)){
                                $err+=1;
                                $ans{VALUE}{ERROR}{$err}=
                                    "New() unable to find exebin [$tmp] : ".
                                    "FAILED.";
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

        ##
        ## CHECK : POST section
        ##         we loop check on configs / input / output to match
        ##         test conditions from the $INFO section.
        ##
        if (!($err)){
            # t.b.d :
            # nothing actually to check in current example.
        }
    }## //if virtual

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

Base implementation for the run method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=item $Bin

The location of perl execution context.

=item \%info

A hash ref to the INFO section from the job.

=item \%env

A hash ref to the ENV section from the test point.

=item \&callback()

A callback to check inputs, returns 0 for success or a string error.

=over 4

=item \%INFO

A hash ref to the INFO section from the test point.

=item \%ENV

A hash ref to the ENV section from the test point.

=back

=item \&callback()

The core execution callback, returns standard JSON reply.

=over 4

=item $Bin

The location of perl execution context.

=item \%INFO

A hash ref to the INFO section from the test point.

=item \%ENV

A hash ref to the ENV section from the test point.

=back

=back

=head3 Return

Standard JSON reply.

=cut
sub run{
    my %ans=();       # anser hash, empry by default
    my $quiet=0;      # FLAG : do we need to run quiet?
    #
    my $err = 0;
    my $tmp;
    my $logMsg;

    my @cmd = split(/:/,$_[0]);
    my $Bin = $_[1];
    my %INFO = %{$_[2]};
    my %ENV = %{$_[3]};
    my $inputsCallback = $_[4];
    my $callback = $_[5];

    ### Get the option for the CMD, if any.
    my $opt = getActionOpt(@cmd);
    if ($opt eq "q" ){
        $quiet=1;
    }

    # sanity check
    if (lc($INFO{TYPE}) eq "real")
    {
        # prepare foldernames
        my $tstabspath = $Bin."/".$ENV{TSTRELPATH}."/";
        my $jobabspath = $Bin."/".$ENV{JOBRELPATH}."/";
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
            for my $datakey ( keys %{$ENV{INPUT}{DATA}} ) {
                # check for presence requested??
                if (exists 
                    $ENV{VALUE}{INPUT}{DATA}{$datakey}{CHECK}{RUN}{PRE}{PRESENCE}){
                    $tmp =  $ENV{INPUT}{$datakey}{PATH}."/".
                        $ENV{INPUT}{$datakey}{FILE};
                    if (!(-f $tmp)){
                        $err+=1;
                        $ans{VALUE}{ERROR}{$err}=
                            "Run() missing input [$tmp] : FAILED.";
                    }
                }
            }

            my $ret = $inputsCallback->(\%INFO, \%ENV);
            if ($ret ne "0")
            {
                $err+=1;
                $ans{VALUE}{ERROR}{$err}=$ret;
            }
            
            # check the system type
            # (needed for cygwin/window and POSIX filenames)
            my $OS_type = detectOSType(\%ENV);
            if (lc($OS_type) eq "none"){
                $err+=1;
                $ans{VALUE}{ERROR}{$err}="unknown OS type.[$tmp]";
            }

            ##
            ## Core TestPoint Execution
            ##
            if(!($err)){
                $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
                $logMsg = "Run , START , ".getDateTimeNow();
                logFileDump($tmp,$logMsg);

		my $ran = $callback->($Bin, \%INFO, \%ENV);
                if ($ran->{STATUS} ne "0")
                {
                    $err+=1;
                    # FIXIT first error?!?!?!
                    $ans{VALUE}{ERROR}{$err}=$ran->{VALUE}{ERROR}{"0"};
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
    }
    else{
        $err+=1;
        $ans{VALUE}{ERROR}{$err}="virtual TPT, cannot be run";
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
        $ans{VALUE}{ERROR}{$err}="TPT.Run : FAILED.";
        $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
        $logMsg = "Run , FAILED , ".getDateTimeNow();
        logFileDump($tmp,$logMsg);
    }

    # return
    JSONLog("jTXT",\%ans) if (!$quiet);
    # always return an hash
    return \%ans;
}
# }}}

=head2 finalize()

Base implementation for the finalize method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=item $Bin

The location of perl execution context.

=item \%info

A hash ref to the INFO section from the test point.

=item \%env

A hash ref to the ENV section from the test point.

=back

=head3 Return

Standard JSON reply.

=cut
sub finalize{

    my $err = 0;
    my %ans=();       # anser hash, empry by default
    my $quiet=0;      # FLAG : do we need to run quiet?
    #
    my $tmp;
    my $logMsg;

    my @cmd = split(/:/,$_[0]);
    my %INFO = %{$_[1]};
    my %ENV = %{$_[2]};

    ### Get the option for the CMD, if any.
    my $opt = getActionOpt(@cmd);
    if ($opt eq "q" ){
        $quiet=1;
    }

    # sanity check
    if (lc($INFO{TYPE}) eq "virtual"){
        # virtual TPT cannot control input
        $err+=1;
        $ans{VALUE}{ERROR}{$err}="virtual TPT, cannot Finalize";
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
                    "Finalize : TPT did not Run correctly.";
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
                    ## t.b.d
                }

                ##
                ## Finalize : Core Code Section
                ##
                if(!($err)){
                    ## t.b.d
                    # add the code to perform check on input/ouput section
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
# }}}

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
    my %ans=();       # anser hash, empry by default
    my $quiet=0;      # FLAG : do we need to run quiet?
    my $err = 0;

    my @cmd = split(/:/,$_[0]);

    ### Get the option for the CMD, if any.
    my $opt = getActionOpt(@cmd);
    if ($opt eq "q"){
        $quiet=1;
    }

    # TPT does not have a workflow!!
    $err+=1;
    $ans{STATUS}="-1";
    $ans{VALUE}{ERROR}{$err}="TPTs does have only ONE worklist : TPT.Run";

    JSONLog("jTXT",\%ans) if (!$quiet);
    # always return an hash
    return \%ans;
}

# }}}

# SUBROUTINE :
# {{{ detectOSType

# Purpose: detect OS
#
#
# Params : ENV hash ref
#
sub detectOSType{

    my %ENV = %{$_[0]};

    # check the system type
    # (needed for cygwin/window and POSIX filenames)
    my $OS_type = "none";
    my $uname = $ENV{SYSBIN}{UNAME}{CMD};
    # FIXIT this is REALLY sloppy - not all tpts have a uname key,
    # so this fails; it is not really generic code.  FIXIT
    return "do not care" if (!defined $uname);
    my $OS_cmd = $uname." -a";
    my $tmp = `$OS_cmd`;
    if (lc($tmp) =~ /^cygwin/){
        $OS_type = "cygwin";
    }
    if (lc($tmp) =~ /^darwin/){
        $OS_type = "macosx";
    }
    if (lc($tmp) =~ /^linux/){
        $OS_type = "linux";
    }
    return $OS_type;
}
# }}}

=head1 COPYRIGHT

Copyright 2011 Gianluca Filippini, Kevin McIntire
All Rights Reserved

=cut


##needed for "require" use
1;

__END__

    
