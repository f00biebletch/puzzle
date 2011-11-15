:
    eval '(exit $?0)' && eval 'exec perl -S $0 ${1+"$@"}' && eval 'exec perl -S $0 $argv:q' if 0;

#
# Copyright <A9> 2011 Gianluca Filippini, Kevin McIntire
# All Rights Reserved
#


###############################################################################
#
# TEST POINT DEFINTION:
#
# A test point does:
#   takes 1 (ONLY one) input vector
#   perform 1 (ONLY one) atomic set of steps (bin execution)
#   generate 1 (ONLY one) output vector
#
###############################################################################



# NOTE : the environment is the only one that can be overwritten when the TPT is
#        dynamically generated. The environment is a combination of TPT_INFO and
#
# {{{ Environment Section
#<HEADER><BEGIN>
#<ENV><BEGIN>
my %ENV = (
    # this is a skeleton of a automatic generated TPT
    # so by default this is a "virtual" TPT.
    # for a runnable TPT this should be "real"
    TYPE => "virtual",

    # Name, Title, Detail can be overwritten (RW values)
    # Note: SPACES OR SPECIAL CHARS (-_$#@etc.) are NOT
    #       allowed for the name definition.
    NAME => "tpt_count",
    TITLE => "count occurrances in a log file",
    DETAIL => "count and sort for highest counts",

    # Each TPT needs to be able to locate the TST folder in relation
    # to the filesystem position. By default this position is "../../"
    # but if the TPT uses a SANDBOX we need to modify this value to be
    # able to reach the TST folder from the sandbox.
    # NOTE : the TSTABSPATH can be computes as $Bin./$ENV{TSTRELPATH}
    #       during the Job.New call.
    #
    TSTRELPATH=>"../../../",
    JOBRELPATH=>"../../",

    # TPT Test Binary Section
    # files to be used during TPT run
    # The TPT binaries are always in the default directory $TSTABSPATH/bin
    # NOTE: self populated by Job New()
    #
    EXEBIN=>{
    },

    # System Binary section.
    # NOTE: self populated by Job New()
    #
    SYSBIN=>{
    },

    # Configs section : required system commands, absolute path etc.
    # NOTE: self populated by Job New()
    #
    CONFIG=>{
    },

    # Input section : all required input vectors by the configs used.
    # NOTE: self populated by Job New()
    #
    INPUT=>{
    },

    # Output section : all required output vectors by the configs used.
    # NOTE: self populated by Job New()
    #
    OUTPUT=>{
    },

    # sandbox output location, used to run tmp process
    # NOTE: self populated by Job New()
    #
    SANDBOX=>{
    },

    );
#<ENV><END>
# }}}


# {{{ Information section
# NOTE : the info section is a STATIC READ ONLY section and should not be 
# modified when
#        the TPT is dynamically generated. All the informations that need 
#      to be modified
#        will be written into the ENV section.
#        the flowcontrol for a "virtual" TPT will be:
#        1) the JOB looks for TPTs
#        2) if the TPT is virtual the JOB will ask for TPT.Info
#        3) based on TPT.Info results the JOB (I repeat : the JOB) will 
#           create the "real" TPTs
#        4) the real TPT will have all the overwritten info into the ENV section
#

#<INFO><BEGIN>
my %INFO = (
    # TPT object definition
    #
    # Priority, APIversion and VERSION must be considered READONLY
    # and cannot be overwritten by the ENV. section
    # these values cannot be changed during the creation of a real TPT
    # from the virtual TPT template.
    #
    PRIORITY => "10",         # execution (by the parent) priority order
    VERSION  => "1.0.0",     # this is the object version, i.e. TPT version
    APIVERSION => "0.1.0",   # this is the API version followed by this object

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
    # NOTE : system requirements are checked by the owner JOB
    #        and checked again on TPT.New
    #
    OS => {BRAND=>"linux",
           TYPE=>"any",
           VERSION=>{MAJOR=>"any",MINOR=>"any"},
           BIT => "any",
           CHECK=>"eq"},

    # Hardware, use "any" for don't care condition
    #
    HW => {ARCH =>"any",
           ISET =>"any",
           CPU=>{BRAND=>"any",NAME=>"any",CODENAME=>"any",MODEL=>"any"},
           CHECK=>"eq"},

    # Physical resources in MBytes
    #
    RESOURCES => {RAM => "2000", INPUT => "100", OUTPUT => "100", 
                  UNIT=>"Mbyte", CHECK=>"ge"},

    # There is one more requirement for the Job : CLUSTER
    CLUSTER => {TYPE=>"any"},

    # files to be used during TPT run
    #
    EXEBIN=>{
        # data section
        DATA=>{
            # TPT will encode/decode as atomic operation
            
            },
        
    },

    # required system commands, used during TPT run
    #
    SYSBIN=>{
        UNAME=>{NAME=>"UName",CMD=>"uname",VERCMD=>"--version",VER=>"1.0",
                CHECK=>"any"},
        OCTAVE=>{NAME=>"Octave",CMD=>"octave",VERCMD=>"-v",VER=>"2.9",CHECK=>"ge"},
        DIFF=>{NAME=>"FileDiffer",CMD=>"diff",VERCMD=>"-v",VER=>"2.8.7",
               CHECK=>"ge"},
        CP=>{NAME=>"FileCopy",CMD=>"cp",VERCMD=>"--version",VER=>"6.10",
             CHECK=>"ge"},
        RM=>{NAME=>"Remove",CMD=>"rm",VERCMD=>"",VER=>"",CHECK=>""},
        TAR=>{NAME=>"tar",CMD=>"tar",VERCMD=>"--version",VER=>"1.0",CHECK=>""},
        YES=>{NAME=>"YESRepeat",CMD=>"yes",VERCMD=>"",VER=>"",CHECK=>""},
    },

    # configuraton vector for the EXEBIN
    # the physical folder for these files will be always 
    # tst/configs/$INFO{CONFIG}{DIR}
    # Note that these values are crosschecked/generated from the Job
    # during the Job.New call.
    #
    CONFIG=>{# I don;t need a subfolder for the config section of this example.
        DIR=>"",
        # data section
        DATA=>{

        },
    },

    # input vector for the EXEBIN
    # the physical folder for these files will be always 
    # tst/input/$INFO{INPUT}{DIR}
    # Input section : all required input vectors by the configs used.
    # NOTE : this section will not be empty even if the same info are
    #        carried by the configs file, because we still have to
    #        specify the informations to CHECK, i.e. scatter/gather etc.
    #        ONLY if no extra info are needed, this section might be empty
    #
    INPUT=>{
        # NOTE: input path is ALWAYS hardcoded.
        # NOTE : absolute path will built as:
        #        $ENV{TST}/$ENV{INPUT}{PATH}/
        # subfolder for TPT
        DIR=>"",
        # data section
        DATA=>{

        },
    },

    # Output section : all required output vectors by the configs used.
    # the physical folder for these files will be always 
    # tst/output/$INFO{OUTPUT}{DIR}
    # NOTE : this section will not be empty even if the same info are
    #        carried by the configs file, because we still have to
    #        specify the informations to CHECK, i.e. scatter/gather etc.
    #        ONLY if no extra info are needed, this section might be empty
    #        and all the output files will be gathered by the Job.Finalize
    #        at the completion of all the TPTs.
    OUTPUT=>{
        # NOTE: output path is ALWAYS used to owerwirte 
        # $ENV{OUTPUT}{PATH}=...../TPT{name}
        #       during the TPT.New call..
        DIR=>$ENV{NAME},

        # data section is non empty only if we have data dependency in between
        # tpts or if we have checks to do on the ouput for 
        # Pre/Post NEW/RUN/FINALIZE
        #
        DATA=>{

        },
    },

    # sandbox output location, used to run tmp process
    #
    SANDBOX=>{
        # this folder is created during the TPT.New call.
        # and will be always under the OUTPUT folder since
        # we need output read/write rights.
        # NOTE : if the process does not use a sandbox
        #        this might be empty.
        # NOTE : sandbox is removed by the TPT.Finalize call.
        DIR=>"tptsbox",
        CHECK=>{},
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
use JSON;
use Time::HiRes;
use Data::Dumper;

#
# use modules local
#
use lib $Bin.'/../../inc/';
use SimpleLog;
use JSONLog;
use SysTool;
use SysCheck;
use rawFileParse;
use pzmApiControl;

use Tpt;

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
# {{{ TestPoint : Info
################################
#
sub info{
    logit($verbose,LOG_SIMPLE,WHITE,"TPT:info\n");

    return Tpt::info($_[0], \%INFO);
}
# }}}

#
################################
# API METHOD :
# {{{ TestPoint : New
################################
#
sub new{
    return Tpt::new($_[0], $Bin, \%INFO, \%ENV);
}
# }}}


#
################################
# API METHOD :
# {{{ TestPoint : workflow
################################
#
sub workflow{
    return Tpt::workflow($_[0]);
}
# }}}


#
################################
# API METHOD :
# {{{ TestPoint : worklist

################################
#
sub worklist{
    return Tpt::worklist($_[0]);
}

# }}}


#
################################
# METHOD :
# {{{ TestPoint : Run
################################
#
sub run{
    return Tpt::run($_[0], $Bin, \%INFO, \%ENV, 
                    \&checkInputs, \&do_run);
}
# }}}

sub checkInputs{
    
    my %INFO = %{$_[0]};
    my %ENV = %{$_[1]};

    # check the system type
    # (needed for cygwin/window and POSIX filenames)
    my $OS_type = detectOSType();
    if (lc($OS_type) eq "none"){
        return "unknown OS type.[$OS_type]";
    }

    return 0;
}

#select the top 10 counts for the hash 
sub top {
    my %inhash = %{$_[0]};
    my $count = 0;
    my @keys = ();
    my $min = 0;
    my $tlct = 0;
    for my $ival (keys %inhash) {
        
      if ($inhash{$ival} > $min) {
            push(@keys, $ival);
            @keys = sort {$inhash{$b} <=> $inhash{$a}} @keys;
            pop(@keys) if ($#keys >= 10);
            if ($tlct > 9) {
                $min = $inhash{$keys[-1]};
            }
            $tlct += 1;
        }
    }
    return @keys;
}

my %ubytes;
my %ucount;
my %uhits;
my %uclients;
my $uctr = 0;
my %urefs;

#add to the hash and increment counters
sub record {
    my ($client,$u,$bytes,$ref) = @_;
    $ubytes{$u} += $bytes;
    $uctr += 1;
    $uhits{$u} += 1;
    $uclients{$client} += 1;
    $urefs{substr($ref,1,-2)} += 1;

}#end record

#main processing loop
sub do_run{
    my $Bin = $_[0];
    my %INFO = %{$_[1]};
    my %ENV = %{$_[2]};
    my $seqname;
    my $strnum;
    my $err = 0;
    my %ans = ();

    my $tstabspath = $Bin."/".$ENV{TSTRELPATH}."/";

    # LogFile update
    my $tmp = $ENV{OUTPUT}{PATH}."/".$ENV{NAME}.".log";
    my $logMsg = "Count , START , ".getDateTimeNow();
    logFileDump($tmp,$logMsg);
    #

    my $filein = $INFO{INPUT}{DATA}{INPUT}{DIR}."/".$INFO{INPUT}{DATA}{INPUT}{FILE}; 

        # copy the summary log files into the tpt dir
        my $myKey;
        my ($client,$u,$status,$bytes,$ref);
        my %ct404;

            open(LFILE, "<$filein") || die "cannot open $filein: $!\n";
                while (<LFILE>) {
                    my @sline = split(/\s+/, $_);
                    next unless ($sline[5] eq '"GET');
                    $client = $sline[0];
                    $u = $sline[6];
                    $status = $sline[8];
                    $bytes = $sline[9];
                    $ref = $sline[10];
                    if ($status eq '200' and $bytes ne "-") {
                        record($client,$u,int($bytes),$ref);
                    }
                    elsif ($status eq '304') {
                        record($client,$u,0,$ref);
                    }
                    elsif ($status eq '404') {
                      $ct404{$u} += 1;
                    }
                }
            close(LFILE);

            open(FOUT, ">$ENV{OUTPUT}{PATH}"."/".$INFO{OUTPUT}{DATA}{RESULTS_LOG}{FILE}) or die "cannot open output file: $!\n";

#print results to the output file

my @top10 = top(\%uhits);
print FOUT "URIs by hit\n";
for my $t10 (@top10) {
    print FOUT "key $t10 count= $uhits{$t10}\n";
    }

@top10 = top(\%ucount);
print FOUT "URIs by ucount\n";
for my $t10 (@top10) {
    print FOUT "key $t10 count= $ucount{$t10}\n";
    }

@top10 = top(\%uclients);
print FOUT "URIs by uclient\n";
for my $t10 (@top10) {
    print FOUT "key $t10 count= $uclients{$t10}\n";
    }

@top10 = top(\%urefs);
print FOUT "URIs by urefs\n";
for my $t10 (@top10) {
    print FOUT "key $t10 count= $urefs{$t10}\n";
    }

@top10 = top(\%ubytes);
print FOUT "URIs by byte\n";
for my $t10 (@top10) {
    print FOUT "key $t10 count= $ubytes{$t10}\n";
    }

close(FOUT);

$ans{STATUS} = 0;
 return \%ans;
}


# SUBROUTINE :
# {{{ detectOSType

# Purpose: detect OS
#
#
# Params : none
#
sub detectOSType{

    # check the system type
    # (needed for cygwin/window and POSIX filenames)
    my $OS_type = "none";
    my $OS_cmd = $ENV{SYSBIN}{UNAME}{CMD}." -a";
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

#
################################
# METHOD :
# {{{ TestPoint : Finalize
################################
#
sub finalize{
    return Tpt::finalize($_[0], \%INFO, \%ENV);
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

    Test Point (TPT) script - single component of a TestJOB (JOB) batch.

    =head1 SYNOPSIS

#> ./tpt_name.pl -option action{:command:value}

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

    =head1 EXAMPLE

    show the TPT info structure in single line JSON format

    $> tpt_name.pl -j -a info:all

    show the TPT priority info

    $> tpt_name.pl -j -a info:priority


    =head1 OPTIONS

    =over 8

    =item B<-help>

    Print a brief help message and exits.

    =item B<-man>

    Prints the manual page and exits.


    =item B<-verbose {0-3}>

    Set the level of verbosity. 0: none, 3:all

    =back

    =head1 DESCRIPTION

    TPT long description HERE.
    TPT long description HERE.
    TPT long description HERE.
    TPT long description HERE.
    TPT long description HERE.
    TPT long description HERE.
    TPT long description HERE.
    TPT long description HERE.
    TPT long description HERE.
    TPT long description HERE.
    TPT long description HERE.
    TPT long description HERE.
    TPT long description HERE.
    TPT long description HERE.
    TPT long description HERE.
    TPT long description HERE.
    TPT long description HERE.

    =cut

