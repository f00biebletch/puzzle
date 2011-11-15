:
    eval '(exit $?0)' && eval 'exec perl -S $0 ${1+"$@"}' && eval 'exec perl -S $0 $argv:q' if 0;

#
# Copyright <A9> 2010 Dolby Laboratories, Inc.
# All Rights Reserved
#
#
# http://www.dolby.com/
#


###############################################################################
#
# JOB  DEFINTION:
#
# A JOB does:
#   takes 1 or more input vector
#   perform 1 or more atomic set of steps (bin execution)
#   generate 1 or more output vector
#
# see : http://wiki.eng-scl.dolby.net://
#
###############################################################################



# NOTE : the environment is the only one that can be overwritten when the JOB is
#        dynamically generated. The environment is a combination of JOB_INFO and
#        TST_INFO data. See wiki.eng-scl.dolby.net for details.
#
# {{{ Environment Section

#<HEADER><BEGIN>
#<ENV><BEGIN>
my %ENV = (
    # this is a skeleton of a automatic generated JOB
    # so by default this is a "virtual" JOB.
    # for a runnable JOB this should be "real"
    TYPE => "virtual",
    # NOTE : job are virtual for one main reason:
    #        output folder for each TST run must be updated.
    #        usually not much more than this is needed for a
    #        virtual JOB to be translated to real

    # Name, Title, Detail can be overwritten (RW values)
    # Note: SPACES OR SPECIAL CHARS (-_$#@etc.) are NOT
    #       allowed for the name definition.
    NAME => "job_summarize",
    TITLE => "collect results from count and output in JSON",
    DETAIL => "create JSON structure output for log file counts",

    # Each JOB needs to be able to locate the TST folder in relation
    # to the filesystem position. By default this position is "../../"
    # but if the TPT uses a SANDBOX we need to modify this value to be
    # able to reach the TST folder from the sandbox.
    # NOTE : the TSTABSPATH can be computes as $Bin./$ENV{TSTRELPATH}
    #       during the Job.New call.
    #
    TSTRELPATH=>"../",
    JOBRELPATH=>"./",

    # TPT Test Binary Section
    # files to be used during TPT run
    # The TPT binaries are always in the default directory $TSTABSPATH/bin
    # NOTE: self populated by tstApi New()
    #
    EXEBIN=>{
    },

    # System Binary section.
    # NOTE: self populated by tstApi New()
    #
    SYSBIN=>{
    },

    # Configs section : required configuration files to run in this job
    #                   this is a mulitple vector, and the topology
    #                   of connection is hardcoded by the Job.NEW() call.
    # NOTE: self populated by tstApi New()
    #
    CONFIG=>{
    },

    # Input section : all required input vectors by the configs used.
    # NOTE : FILE is an array, always use [], even for FILE=>["all"]
    # NOTE: self populated by tstApi New()
    #
    INPUT=>{
    },

    # Output section : all required output vectors by the configs used.
    # NOTE : FILE is an array, always use [], even for FILE=>["all"]
    # NOTE: self populated by tstApi New()
    #
    OUTPUT=>{
    },

    # sandbox output location, used to run tmp process
    # NOTE: self populated by tstApi New()
    #
    SANDBOX=>{
    },

    # this is needed by this particular job
    # we spawn multiple TPT with different specific
    # for hw profile (cpu/os)
    # so we keep a list of the hw to call
    HWSELECT=>{
    },
    );
#<ENV><END>

# }}}

# {{{ Information Section

#<INFO><BEGIN>
my %INFO = (
    # JOB object definition
    #
    # Priority, APIversion and VERSION must be considered READONLY
    # and cannot be overwritten by the ENV. section
    # these values cannot be changed during the creation of a real JOB
    # from the virtual JOB template.
    #
    PRIORITY => "20",              # execution (by the parent) priority order
    VERSION => "1.0.0",  # this is the object version, i.e. JOB version
    APIVERSION => "0.1.0",      # this is the API version followed

    # Name, Title, Detail can be overwritten (RW values)
    # this section of the INFO should be always like this
    NAME =>  $ENV{NAME},
    TITLE =>  $ENV{TITLE},
    DETAIL =>  $ENV{DETAIL},

    # this is a skeleton of a automatic generated TPT
    # so by default this is a "virtual" TPT.
    # for a runnable TPT this should be "real"
    # this section of the INFO should be always like this
    TYPE => $ENV{TYPE},
    TSTRELPATH => $ENV{TSTRELPATH},
    JOBRELPATH => $ENV{JOBRELPATH},

    # System Requirements OS, HW, RESOURCES, EXEBIN, SYSBIN
    # Operating System (OS),use "any" for don't care condition
    # NOTE : system requirements for the job itself are usually
    #        very "open", only in few cases the JOB will restrict the hw.
    #        the overall HW requirements should be cross checked with
    #        the requirements of each TPT of the workList.
    # NOTE
    #
    OS => {BRAND=>"linux",
           TYPE=>"any",
           VERSION=>{MAJOR=>"any",MINOR=>"any"},
           BIT => "32",
           CHECK=>"ge"},

    # Hardware, use "any" for don't care condition
    #
    HW => {ARCH =>"x86",
           ISET =>"any",
           CPU=>{BRAND=>"any",NAME=>"any",CODENAME=>"any",MODEL=>"any"},
           CHECK=>"ge"},

    # this is needed by this particular job
    # we spawn multiple TPT with different specific
    # for hw profile (cpu/os)
    # so we keep a list of the hw to call
    HWSELECT=>{
        XN650=>{ARCH =>"x86",
                ISET =>"any",
                CPU=>{BRAND=>"intel",NAME=>"xeon",CODENAME=>"any",MODEL=>"any"},
        },
    },
    # Physical resources in MBytes
    #
    RESOURCES => {RAM => "any", INPUT => "any", OUTPUT => "any", 
                  UNIT=>"Mbyte", CHECK=>"ge"},

    # There is one more requirement for the Job : CLUSTER
    # CLUSTER may have 4 values : "any", "local", "remote", "hw".
    CLUSTER => {TYPE=>"any"},

    # exe files to be used during JOB run
    #
    EXEBIN=>{
        # data section
        DATA=>{
            
        },
    },

    # required system commands, used during TPT run
    #
    SYSBIN=>{
        DIFF=>{NAME=>"FileDiffer",CMD=>"diff",VERCMD=>"-v",VER=>"2.8.7",
               CHECK=>"ge"},
        CP=>{NAME=>"FileCopy",CMD=>"cp",VERCMD=>"--version",VER=>"6.10",
             CHECK=>"ge"},
        RM=>{NAME=>"FileRemove",CMD=>"rm",VERCMD=>"--version",VER=>"0.1",
             CHECK=>""},
    },

    CONFIG=>{
        DIR=>"",
        # NOTE : the JOB config data is a multi vector input.
        #        each vector contains the config data definition for the TPTs
        #

        # NOTE: specify "all" for FILE to use all the data available in the DIR.
        #        If each $KEY{DIR} it's not empty will be used as subdir
        #        full path = $INFO{CONFIG}{DIR}/$INFO{CONFIG}{DATA}{$KEY}{DIR}
        #
        # NOTE : FILE is an array, always use [], even for FILE=>["all"]
        #

        DATA=>{
        },
    },

    INPUT=>{
        DIR=>"",
        # NOTE : the JOB input data is a multi vector input.
        #        each vector contains the input data definition for the TPTs
        #
        # NOTE : FILE is an array, always use [], even for FILE=>["all"]
        #
        #
        # NOTE : convention for the FILE array:
        #        array[0] is the LDR input
        #        array[1] is the HDR input
        DATA=>{
        },
    },

    OUTPUT=>{
        # NOTE: we keep the output folder structur ordered by TST/JOB/TPT
        #       so we always create an output subfolder with the same name
        #       of the Job name itself
        #
        # NOTE : FILE is an array, always use [], even for FILE=>["all"]
        #
        DIR=>$ENV{NAME},
        # data section is non empty only if we have data dependency in between
        # jobs or if we have checks to do on the output for 
        # Pre/Post NEW/RUN/FINALIZE
        DATA=>{
        },
    },
    # sandbox output location, used to run tmp process
    #
    SANDBOX=>{
        # this folder is created during the JOB.New call.
        # and will be always under the OUTPUT folder since
        # we need output read/write rights.
        # NOTE : if the process does not use a sandbox
        #        this might be empty.
        # NOTE : sandbox is removed by the JOB.Finalize call.
        DIR=>"jobsbox",
    },

    # this is needed for the VDR test
    #
    EXTRA => {
    },
    );
#<INFO><END>
#<HEADER><END>

# }}}


###############################################################################
##                  PERL SYSTEM INIT
###############################################################################


#
# use modules
#
use warnings;
use strict;
use Term::ANSIColor qw(:constants);
use Getopt::Long;
use Pod::Usage;
use Switch;
use FindBin qw($Bin);
use Scalar::Util 'reftype';
use JSON;
use Data::Dumper;
use File::Path;

#
# use modules local
#
use lib $Bin.'/../inc/';
use SimpleLog;
use JSONLog;
use SysTool;
use SysCheck;
use RawFileParse;
use pzmApiControl;

use Job;

#
# system parameters
#
my $verbose = LOG_MIN;     # default verbosity level
my $man;                   # man pages
my $help;                  # brief help
my $acmd;                  # action command
my $ccmd;                  # control command
my $json;                  # JSON output format flag
##
my $result;
my $err = 0;
my $jfmt = "jTXT";
##
my %workflow;



###############################################################################
##                    METHODS + SUBROUTINES
###############################################################################

#
################################
# API METHOD :
# {{{ TestJob : Info
################################
#
sub info{
    logit($verbose,LOG_SIMPLE,WHITE,"JOB:info\n");

    return Job::info($_[0], \%INFO);
}
# }}}

#
################################
# API METHOD :
# {{{ TestJob : New

################################
#

sub new{
    return Job::new($_[0], $Bin, \%INFO, \%ENV, \&generate_tpt);
}

# }}}


#
################################
# API METHOD :
# {{{ TestJob : workflow

################################
#
sub workflow{
    logit($verbose,LOG_SIMPLE,WHITE,"JOB:workflow\n");

    return Job::workflow($_[0], $Bin);
}

# }}}

#
################################
# API METHOD :
# {{{ TestJob : worklist
################################
#
sub worklist{
    logit($verbose,LOG_SIMPLE,WHITE,"JOB:worklist\n");

    return Job::worklist($_[0], $Bin);
}
# }}}

#
################################
# API METHOD :
# {{{ TestJob : Run

################################
#
sub run{
    logit($verbose,LOG_SIMPLE,WHITE,"TST.run received\n\n");

    return Job::run($_[0], $Bin, \%INFO, \%ENV);    
}

# }}}


#
################################
# METHOD :
# {{{ TestJob : Finalize

################################
#
sub finalize{
    return Job::finalize($_[0], $Bin, \%INFO, \%ENV);    
}

# }}}

sub generate_tpt{

    my $root = $_[0];
    my $worklist = $_[1];
    my (%INFO) = %{$_[2]};
    my (%ENV) = %{$_[3]};
    my $count = 0;
    my %ans = ();
    my $err = 0;

    my %countWorkflow;

    # get the workflow of job_count
    my $infoCmd = $root."../job_count/job_count.pl -a workflow";
    my @descriptor = `$infoCmd`;
    my $string = join('',@descriptor); chomp($string);
    my $countflow = from_json($string);
    if (defined $countflow->{STATUS}){
        if(lc($countflow->{STATUS}) eq "0"){
            # for each tpt in probe, group by sequence using the summary log
            for my $countTpt ( keys %{$countflow->{VALUE}} ){
                $infoCmd = $root."/../job_count/tpt/".
                    $countflow->{VALUE}{$countTpt}{FILE}." -a info";
                @descriptor = `$infoCmd`;
                $string = join('',@descriptor); chomp($string);
                my $count = from_json($string);
                my $ctpt = "tpt_$countTpt";
                my $seq = 
                    $count->{VALUE}{OUTPUT}{DATA}{RESULTS_LOG}{FILE};
                $countWorkflow{$ctpt} = $seq;

            }
        }
        else{
            $err+=1;
            $ans{VALUE}{ERROR}{$err}=
                "New() : cannot retrieve job_produce workflow [01]";
        }
    }
    else{
        $err+=1;
        $ans{VALUE}{ERROR}{$err}=
            "New() : cannot retrieve job_produce workflow [02]";
    }

    for my $work ( keys %{$worklist}){
        # get the info of this TPT
        $infoCmd = $root."/tpt/tpt_".$work.".pl -a info";

        # get the info on each virtual tpt of the worklist
        @descriptor = `$infoCmd`;
        # check the status on $tmp[0]
        $string = join('',@descriptor); chomp($string);
        my $tmp = from_json($string);
        if(exists $tmp->{STATUS}){
            if(lc($tmp->{STATUS}) eq (lc("0"))){
                #
                # SET n.01 OF AUTOMATED FILE GENERATION
                #
                # NOTE : in this example the JOB consists in multiple parallel
                #        x264 encodings, so we generate the configs and the TPT
                #        only for the TPT named "x264" with priority "1"
                #
                # NOTE : this is a specific condition of this example
                #

                if(lc($tmp->{VALUE}{NAME} eq "tpt_summarize")) {

                    # need a bkp for title mangling
                    my $title_bkp = substr $tmp->{VALUE}{TITLE}, 0;

                    # inner local vars
                    my $tptname;
                    my $infoTPT;
                    my $input;

                    # get the TPT info structure
                    $infoTPT = $tmp->{VALUE};

                    for my $workKey ( keys %countWorkflow){
                        my $lfile = $countWorkflow{$workKey};
                        $count+=1;

                        $string  = 
                            sprintf("$root/tpt/tpt_$work\_%08d.pl",
                                    $count);
                        $tptname = sprintf("tpt_$work\_%08d",
                                           $count);


                        ### (1) CREATE the TPT file
                        # {{{
                        # OS requirements
                        
                        # we want this exact HW match
                        $infoTPT->{HW}{CHECK}="eq";
                        # modifiy info values on the base of this job
                        $infoTPT->{TYPE}="real";
                        $infoTPT->{NAME}=$tptname;
                        $infoTPT->{TITLE}=$title_bkp.",".
                            $infoTPT->{OS}{BRAND}.",".
                            $infoTPT->{OS}{BIT};
                        $infoTPT->{INPUT}{DIR}="";
                        $infoTPT->{INPUT}{DATA}{COUNT}{FILE} = $lfile;
                        $infoTPT->{INPUT}{DATA}{COUNT}{DIR} = $root."../../output/job_count/$workKey";
                        
                        #
                        $infoTPT->{OUTPUT}{DIR}=$tptname;
                        $infoTPT->{OUTPUT}{DATA}{COUNT}{FILE} = $lfile.".json";
                        #

                        
                        ### CREATE TPT real file.
                        setConfigTPT( $root."/tpt/tpt_".$work.".pl",
                                      $infoTPT,
                                      $root."/tpt/".$tptname.".pl"
                            );
                        # now clear custom keys from memory;
                        delete $infoTPT->{CONFIG}{DATA};
                        delete $infoTPT->{INPUT}{DATA};
                        delete $infoTPT->{OUTPUT}{DATA};
                        delete $infoTPT->{TITLE};

                    }
                }
            }
            else{ 
                $err+=1;
                $ans{VALUE}{ERROR}{$err}="New() [$infoCmd] : FAILED.";
            }
        }
        else{
            $err+=1;
            $ans{VALUE}{ERROR}{$err}=
                "New() error getting TPT info [$infoCmd] : FAILED.";
        }
    } # LOOP : for $work in a worklist
    
    if($err){
        $err+=1;
        $ans{STATUS}="-1";
    }
    else{
        # ALL looks good
        $ans{STATUS}="0";
    }
    # always return an hash
    return \%ans;
}




#
# SUBROUTINE :
# {{{ setConfigTPT Function

# Purpose: create a configuration file for TPT emcpp
#          based on input template TPT and new $INFO struct
#
# Params : $_[0] is the input template file, fullname (path+name)
#          $_[1] is the input $INFO data
#          $_[2] is the output TPT file, fullname (path+name)
#
sub setConfigTPT{
    my $ret = 0;
    my @tptlines;
    my @tptPreHeader;
    my @tptPostHeader;
    #
    my $line;

    # NOTE: by default nothing can be put at the begining of JOB/TPT files other than
    #       ENV and INFO section, so we set the flag to 1 by default
    my $flagPre=1;
    my $flagPost=0;
    my $flag=0;


    logit($verbose,LOG_SIMPLE,WHITE,"setConfigTPT received\n\n");
    if( (defined $_[0]) && (defined $_[1]) && (defined $_[2]) ){

        ## build the new ENV section based on the input INFO section
        ##
        my %TPTENV;
        my $key;
        $TPTENV{TYPE}="real";
        $TPTENV{NAME}=$_[1]->{NAME};
        $TPTENV{TITLE}=$_[1]->{TITLE};
        $TPTENV{DETAIL}=$_[1]->{DETAIL};
        $TPTENV{TSTRELPATH}="../../../";
        $TPTENV{JOBRELPATH}="../../";
        # OS ENV section
        $TPTENV{OS}{BRAND}=$_[1]->{OS}{BRAND};
        $TPTENV{OS}{TYPE}=$_[1]->{OS}{TYPE};
        $TPTENV{OS}{BIT}=$_[1]->{OS}{BIT};

        # EXEBIN ENV section
        for $key ( keys %{$_[1]->{EXEBIN}{DATA}} ){
            $TPTENV{EXEBIN}{$key}{CMD} = $_[1]->{EXEBIN}{DATA}{$key}{CMD};
        }
        # SYSBIN ENV section
        for $key ( keys %{$_[1]->{SYSBIN}} ){
            $TPTENV{SYSBIN}{$key}{CMD} = $_[1]->{SYSBIN}{$key}{CMD};
            $TPTENV{SYSBIN}{$key}{PATH} = "";
        }
        # CONFIG ENV section
        # custom config files are now in the output folder of the job_produce
        $TPTENV{CONFIG}{PATH} = $ENV{OUTPUT}{PATH}."/../".$_[1]->{CONFIG}{DIR};
        for $key ( keys %{$_[1]->{CONFIG}{DATA}} ){
            $TPTENV{CONFIG}{$key}{PATH} = $ENV{OUTPUT}{PATH}."/../".$_[1]->{CONFIG}{DIR}."/".$_[1]->{CONFIG}{DATA}{$key}{DIR};
            $TPTENV{CONFIG}{$key}{FILE} = $_[1]->{CONFIG}{DATA}{$key}{FILE};
        }
        # INPUT ENV section
        $TPTENV{INPUT}{PATH} = $ENV{INPUT}{PATH};
        for $key ( keys %{$_[1]->{INPUT}{DATA}} ){
            $TPTENV{INPUT}{$key}{PATH} = $_[1]->{INPUT}{DATA}{$key}{DIR};
#$TPTENV{INPUT}{$key}{PATH} = $ENV{INPUT}{PATH}; #."/../".$_[1]->{INPUT}{DATA}{$key}{DIR};
            $TPTENV{INPUT}{$key}{FILE} =  $_[1]->{INPUT}{DATA}{$key}{FILE};
        }
        # OUTPUT ENV section
        $TPTENV{OUTPUT}{PATH} = $ENV{OUTPUT}{PATH}."/".$_[1]->{OUTPUT}{DIR};
        for $key ( keys %{$_[1]->{OUTPUT}{DATA}} ){
            $TPTENV{OUTPUT}{$key}{FILE} = $_[1]->{OUTPUT}{DATA}{$key}{FILE};
            $TPTENV{OUTPUT}{$key}{PATH} = $ENV{OUTPUT}{PATH}."/".$_[1]->{OUTPUT}{DIR};
        }
        # SANDBOX ENV section
        $TPTENV{SANDBOX}{PATH}= $ENV{OUTPUT}{PATH}."/".$_[1]->{OUTPUT}{DIR}."/".$_[1]->{SANDBOX}{DIR};


        ## read the template TPT file
        ##
        open MYTPT, "<", $_[0];
        @tptlines = <MYTPT>;
        close MYTPT;

        ## parse the file and remove the ENV and INFO section.
        ##
        for $line (@tptlines){
            #
            # note I don;t do any sanity check here
            #      on INFO and ENV unique count
            #      t.b.d : making sure we have only one ENV and one INFO
            #
            if(($line =~ /HEADER/)&&($line=~/BEGIN/)) {$flag = 1; $flagPre = 0;};
            if(($line =~ /HEADER/)&&($line=~/END/)) {$flag = 0; $flagPost= 1;};

            #print $flag."[".$line."\n";#$line."\n" if($flag);
            push(@tptPreHeader,  $line) if($flagPre);
            push(@tptPostHeader, $line) if($flagPost);

        }

        ## Create the TPT file
        ##
        # dump the pre header section
        #
        open MYTPT, ">", $_[2];
        $line =join("",@tptPreHeader);
        print MYTPT $line;

        # now dump the %ENV section
        #
        $Data::Dumper::Useqq  = 1;
        $line = Data::Dumper->Dump([ \%TPTENV ], [ '%ENV' ]);
        $line = substr $line, 1; # dumper fix   : remove first char
        $line = "my ".$line;     # dumper trick : add the declarator
        $line =~ s/{/(/;         # dumper fix for "\{"
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
        #open MYTPT, ">>", $_[2];
        $line =join("",@tptPostHeader);
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

# }}}


###############################################################################
##                    MAIN
###############################################################################

#
# Parse input vector
#
GetOptions( 'help|?'=>\$help,
            man=>\$man,
            'a=s' => \$acmd,
            'c=s' => \$ccmd,
            'json|j' => \$json,
            "verbose=i" => \$verbose)  or  pod2usage(-verbose => 0);

pod2usage(-verbose => 1)  if ($help);
pod2usage(-exitstatus => 0, -verbose => 2)  if ($man);

#
# Mandatory check for JSON return formatting
#
if($json)
{ $verbose=LOG_NONE; $jfmt="jJSON"; }
else
{ $jfmt="jTXT"; }


CopyRight() if ($verbose>LOG_SIMPLE);

#
# Check for single command sintax
#
logit($verbose,LOG_DETAILS,BLUE,"# Check for single command sintax\n");
pod2usage(-verbose => 2, -message => "$0: Too many commands given.\n") if ((@ARGV > 1)||(@ARGV > 1));
#

#
# Sanity checks for command line input parameters
#
# \\none, t.b.implemented
logit($verbose,LOG_DETAILS,BLUE,"# Sanity check for input parameter\n");


#
# action command section
#
if ($acmd){
    my @cmd;
    chomp($acmd);
    @cmd = split(/:/,$acmd);
    logit($verbose,LOG_DETAILS,BLUE,"# action command section -> $cmd[0] {$acmd}\n");
    switch (lc($cmd[0])){
        case "info"     { info($acmd); }
        case "new"      { new($acmd); }
        case "workflow" { workflow($acmd); }
        case "worklist" { worklist($acmd); }
        case "run"      { run($acmd); }
        case "finalize" { finalize($acmd); }
        # default
        else { logit(LOG_DETAILS,LOG_NONE,RED,"Unknonwn Action Command. EXITING.\n"); $err+=1; }
    }
}


#
# control command section
#
if ($ccmd){
    pzmApiCtrl($ccmd, \$err, \$verbose, \%ENV, \%INFO);
}





__END__



###############################################################################
##                    HELP embedded section text
###############################################################################


    =head1 NAME

    Test Suite (TST) script - test regression for mainline svn commits.

    =head1 SYNOPSIS

#> ./tstctrl.pl -option {command/value}

    === General Options ===
    -help               brief help message.
    -man                full documentation.
    -a act[:cmd[:opt]]  action command.
    -c ctrl[:cmd[:opt]] control command on a specific section.
    -verbose [val]      set verbose level [0:none, 99:max]
    -j                  use JSON format, no pretty printing

    == Action List ==
    info      : print TPT info set.
    new       : prepare TPT
    workflow  : (virtual) workflow is empty for a TPT !!
    worklist  : (virtual) worklist is empty for a TPT !!
    run       : run TPT
    finalize  : finalize TPT, send "done" message..

    == Control List ==
    ver                 : return TST/JOB/TPT version.
    input[:cmd[:opt]]   : command related to INPUT section.
    config[:cmd[:opt]]  : command related to CONFIG section.
    output[:cmd[:opt]]  : command related to OUTPUT section.
    system[:cmd[:opt]]  : command related to SYSTEM section.
    sandbox[:cmd[:opt]] : command related to SANDBOX section.
    log[:cmd[:opt]]     : command related to logfile.

    = ctrl commands on section : input =
    help[:opt]       : help
    setup[:opt]      : setup the filesystem for the /input folder
    import[:opt]     : import local files into the /input folder

    = ctrl commands on section : config =
    help[:opt]       : help
    setup[:opt]      : setup the filesystem for the /config folder
    import[:opt]     : import local files into the /config folder

    = ctrl commands on section : output =
    help[:opt]       : help
    setup[:opt]      : setup the filesystem for the /output folder
    import[:opt]     : import local files into the /output folder
    cleanup[:opt]    : clean elements of the /output folder
    move[:opt]       : move elements of the /output folder (abstract method)

    = ctrl commands on section : system =
    help[:opt]       : help
    setup[:opt]      : execute a parsed customized command.
    syscall[:opt]    : return the pid for spawned execution item of a child obj.

    = ctrl commands on section : sandbox =
    help[:opt]       : help
    cleanup[:opt]    : clean the filesystem for the /sandbox folder
    move[:opt]       : move elements of the /sandbox folder (abstract method)

    = ctrl commands on section : log =
    help[:opt]       : help
    cleanup[:opt]    : clean the filesystem for the /output/*.log folder
    move[:opt]       : move elements of the /output/*.log files  (abstract method)


    =head1 OPTIONS

    =over 8

    =item B<-help>

    Print a brief help message and exits.

    =item B<-man>

    Prints the manual page and exits.


    =back

    =head1 DESCRIPTION

    JOB long description HERE.
    JOB long description HERE.
    JOB long description HERE.
    JOB long description HERE.
    JOB long description HERE.
    JOB long description HERE.
    JOB long description HERE.
    JOB long description HERE.
    JOB long description HERE.
    JOB long description HERE.
    JOB long description HERE.
    JOB long description HERE.
    JOB long description HERE.
    JOB long description HERE.
    JOB long description HERE.
    JOB long description HERE.
    JOB long description HERE.
    JOB long description HERE.

    =cut

