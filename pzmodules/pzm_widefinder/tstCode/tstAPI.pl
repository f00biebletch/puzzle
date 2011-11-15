:
    eval '(exit $?0)' && eval 'exec perl -S $0 ${1+"$@"}' && eval 'exec perl -S $0 $argv:q' if 0;

#
# Copyright <A9> 2011 Gianluca Filippini, Kevin McIntire
# All Rights Reserved
#
#

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
use Data::Dumper;
use File::Path;

#
# use modules local
#
use lib $Bin.'/./inc/';
#use lib $Bin.'/./lib/';
use pzmApiControl;

use Tst;

###############################################################################
#
# TEST SUITE  DEFINTION:
#
# A JOB does:
#   takes 1 or more input vector
#   perform 1 or more atomic set of steps (bin execution)
#   generate 1 or more output vector
#
###############################################################################

#
# TEST SUITE INNER API LAYER
#

my %ENV = (
    # TestSuite is ALWAYS a "real" object since it's the root of 
    # the obj. tree.
    TYPE => "real",
    #
    NAME => "WideFinder",
    TITLE => "WideFinder on Puzzle",
    DETAIL => "parsing log files",
    VERSION => "1.0.0",      # this is the object version, i.e. TPT version
    APIVERSION => "0.1.0",   # this is the API version followed by this object
    #
    TSTRELPATH => "./",
    JOBRELPATH => "./",
    ABSPATH => $Bin,
    #
    OUTPUT =>{ PATH => $Bin."/../output/" },
    INPUT  =>{ PATH => $Bin."/../input/" },
    CONFIG =>{ PATH => $Bin."/../configs/" },
    SANDBOX =>{ PATH => $Bin."/../output/tstsbox/"},
    #
    SYSBIN=>{ RM=>{CMD=>"rm",PATH=>""}, },
    );


my %INFO = (
    #
    # Priority, APIversion and VERSION must be considered READONLY
    # and cannot be overwritten by the ENV. section
    # these values cannot be changed during the creation of a real JOB
    # from the virtual JOB template.
    #
    PRIORITY => "999",              # execution (by the parent) priority order
    VERSION => $ENV{VERSION},    # this is the object version, i.e. JOB version
    APIVERSION => $ENV{APIVERSION}, # this is the API version followed by self

    # Name, Title, Detail can be overwritten (RW values)
    # this section of the INFO should be always like this
    NAME => $ENV{NAME},
    TITLE => $ENV{TITLE},
    DETAIL => $ENV{DETAIL},
    MODULE => $ENV{MODULE},

    # this is a skeleton of a automatic generated TPT
    # so by default this is a "virtual" TPT.
    # for a runnable TPT this should be "real"
    # this section of the INFO should be always like this
    TYPE => $ENV{TYPE},
    TSTRELPATH => $ENV{ABSPATH},
    #
    #
    MODULE => {
    },
    EXEBIN => {
    },
    #
    # required system commands,
    #
    SYSBIN=>{
        GREP=>{NAME=>"Grep",CMD=>"grep",VERCMD=>"-V",VER=>"2.4.0",CHECK=>"ge"},
        SED=>{NAME=>"Sed",CMD=>"sed",VERCMD=>"--version",VER=>"1.0.0",
              CHECK=>"ge"},
        GZIP=>{NAME=>"Parallel Gzip",CMD=>"pgzip",VERCMD=>"--version",
               VER=>"2.1.0",CHECK=>"ge"},
        FIND=>{NAME=>"Find",CMD=>"find",VERCMD=>"--version",VER=>"1.0.0",
               CHECK=>"ge"},
        RM=>{NAME=>"FileRemove",CMD=>"rm",VERCMD=>"--version",VER=>"0.1",
             CHECK=>""},
        WGET=>{NAME=>"wget",CMD=>"wget",VERCMD=>"--version",VER=>"1.11.0",
               CHECK=>""},
        TAR=>{NAME=>"tar",CMD=>"tar",VERCMD=>"--version",VER=>"1.0",CHECK=>""},
    },
    #
    #
    OUTPUT =>{ PATH=>$ENV{OUTPUT}{PATH} },
    INPUT  =>{ PATH=>$ENV{INPUT}{PATH} },
    CONFIG =>{ PATH=>$ENV{CONFIG}{PATH} },
    SANBOX =>{ PATH=>$ENV{SANDBOX}{PATH} },

    #
    EXTRA => {    },

    );

###############################################################################
##                  PERL SYSTEM LOCAL DEF.
###############################################################################


#
# use modules local
#
use lib $Bin.'/./inc/';
use SimpleLog;
use JSONLog;
use SysTool;
use SysCheck;
use rawFileParse;

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


###############################################################################
##                    METHODS + SUBROUTINES
###############################################################################

#
################################
# METHOD :
# {{{ TestSuite : Info

################################
#
sub info{
    logit($verbose,LOG_SIMPLE,WHITE,"TST:info\n");
    
    return Tst::info($_[0], \%INFO);
}

# }}}

#
################################
# METHOD :
# {{{ TestSuite : New

################################
#
sub new{
    return Tst::new($_[0], $Bin, \%INFO, \%ENV, \&deploy);
}

# }}}


#
################################
# METHOD :
# {{{ TestSuite : workflow

################################
#
sub workflow{
    logit($verbose,LOG_SIMPLE,WHITE,"JOB:workflow\n");

    return Tst::workflow($_[0], $Bin);
}

# }}}


#
################################
# API METHOD :
# {{{ TestSuite : worklist

################################
#
sub worklist{
    logit($verbose,LOG_SIMPLE,WHITE,"JOB:worklist\n");

    return Tst::worklist($_[0], $Bin);
}

# }}}


#
################################
# METHOD :
# {{{ TestSuite : Run

################################
#
sub run{
    logit($verbose,LOG_SIMPLE,WHITE,"TST.run received\n\n");

    return Tst::run($_[0], $Bin, \%INFO, \%ENV);
}

# }}}


#
################################
# METHOD :
# {{{ TestSuite : Finalize
#
################################
#
sub finalize{
    logit($verbose,LOG_SIMPLE,WHITE,"TST.finalize received\n\n");

    return Tst::finalize($_[0], $Bin, \%INFO, \%ENV, 1, \&render);
}

# }}}

sub deploy{

    my $root = $_[0];
    my $info = $_[1];

    return 0;
}

sub render {
    my $tstabspath = $_[0];
    my $jobabspath = $_[1];
    my $info = $_[2];
    my $env = $_[3];
    my $tptWorkflow;
    my $tmp;
    my $infoCmd;
    my $outFile;
    my @descriptor;
    my $tptinfo;
    my $pstr="";
    my @tptList;
    my @jobList;
    my @pngfiles;
    my $testbed_qp;
    my $qopt;
    my $seqname;
    my %ans;
    my $jobWorkflow;
    my $tstWorkflow;
    my $probeWorkflow;
    my %jhash;
    my $tptseq;

    $jhash{application} = {};

    $outFile = $tstabspath."/../tstView/index.json";
    open(VDRD, ">",$outFile) || die "cannot open $outFile, $!\n";

   close(VDRD); 

   return 0;
}#end render



#
# SUBROUTINE :
# {{{ openHtmlHeader

# Purpose:
#
# Params :
sub openHtmlHeader{
    if (defined($_[0])){
        open MYTSTVIEW, ">", $_[0];

        print MYTSTVIEW "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML ".
            "4.01 Transitional//EN\">\n";
        print MYTSTVIEW "<html>\n";
        print MYTSTVIEW "<head>\n";
        print MYTSTVIEW "<meta http-equiv=\"Content-Type\" ".
            "content=\"text/html; charset=iso-8859-1\">\n";
        print MYTSTVIEW "<meta name=\"GENERATOR\" ".
            "content=\"Arachnophilia 3.9\">\n";
        print MYTSTVIEW "<meta name=\"description\" content=\"\">\n";
        print MYTSTVIEW "<meta name=\"keywords\" content=\"HTML, tags, ".
            "commands\">\n";
        print MYTSTVIEW "<link href=\"css/style.css\" rel=\"stylesheet\" ".
            "type=\"text/css\">\n";

        close MYTSTVIEW;
    }
}

# }}}

#
# SUBROUTINE :
# {{{ closeHtmlHeader
# Purpose:
#
# Params :
sub closeHtmlHeader{
    if (defined($_[0])){
        open MYTSTVIEW, ">>", $_[0];
        print MYTSTVIEW "</head>\n";
        close MYTSTVIEW;
    }
}
# }}}

#
# SUBROUTINE :
# {{{ setHtmlTitle
# Purpose:
#
# Params :
sub setHtmlTitle{
    if (defined($_[0]) && defined($_[1])){
        open MYTSTVIEW, ">>", $_[0];
        print MYTSTVIEW "<title>".$_[1]."</title>\n";
        close MYTSTVIEW;
    }
}
# }}}

#
# SUBROUTINE :
# {{{ setHtmlPuzzleHeader
# Purpose:
#
# Params :
sub setHtmlPuzzleHeader{
    if (defined($_[0])){
        open MYTSTVIEW, ">>", $_[0];
        print MYTSTVIEW "<body>\n";
        print MYTSTVIEW "<div id=\"PuzzleHeader\">\n";
        print MYTSTVIEW "<H5> {fs} Puzzle 2011\n";
        print MYTSTVIEW "</H5> <HR>\n";
        print MYTSTVIEW "</div>\n";

        close MYTSTVIEW;
    }
}
# }}}

#
# SUBROUTINE :
# {{{ setHtmlPuzzleFooter
# Purpose:
#
# Params :
sub setHtmlPuzzleFooter{
    if (defined($_[0])){
        open MYTSTVIEW, ">>", $_[0];
        print MYTSTVIEW "<div id=\"PuzzleFooter\"> <HR>\n";
        print MYTSTVIEW "</H5> <BR>\n";
        print MYTSTVIEW "</div>\n";
        print MYTSTVIEW "</body>\n";
        close MYTSTVIEW;
    }
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

#> ./tstAPI.pl -option {command/value}

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

    TST long description HERE.
    TST long description HERE.
    TST long description HERE.
    TST long description HERE.
    TST long description HERE.
    TST long description HERE.
    TST long description HERE.
    TST long description HERE.
    TST long description HERE.
    TST long description HERE.
    TST long description HERE.
    TST long description HERE.
    TST long description HERE.
    TST long description HERE.
    TST long description HERE.
    TST long description HERE.
    TST long description HERE.
    TST long description HERE.

    =head1 API_SPECS v.0.1 "RedPanda" 

    add a copy of the spec here
    add a copy of the spec here
    add a copy of the spec here
    add a copy of the spec here
    add a copy of the spec here
    add a copy of the spec here
    add a copy of the spec here
    add a copy of the spec here
    add a copy of the spec here
    add a copy of the spec here
    add a copy of the spec here
    add a copy of the spec here
    add a copy of the spec here
    add a copy of the spec here
    add a copy of the spec here
    add a copy of the spec here
    add a copy of the spec here

    =cut

