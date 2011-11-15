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
package pzmApiControl;

#
# this is the perl module version, NOT the api verison
$VERSION          = "0.3";
#


use strict;
use warnings;
use Term::ANSIColor qw(:constants);
use Exporter;
use JSON;
use Switch;
use SimpleLog;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

@ISA         = qw(Exporter);
@EXPORT = qw( pzmApiCtrl );

#
# API REF
#
my $APIVERSION= "0.1";
my $APIVERSION_CODENAME = "RedPanda";

#
# module globals :only the err and verbose (!)
#
my $pzmApiCtrlErr     = -1;
my $pzmApiCtrlVerbose = -1;

#
# **********************************************************
# {{{ CTRL API Interface
#
# $_[0] : control command string
# $_[1] : $err       (reference)
# $_[2] : $verbose   (reference)
# $_[3] : obj. $ENV  (reference)
# $_[4] : obj. $INFO (reference)
sub pzmApiCtrl{
  if ($_[0] && $_[1] && $_[2] && $_[3] && $_[4]){

    my @cmd;
    my $ccmd = $_[0];
    my $env  = $_[3];
    my $info = $_[4];

    # set the globals
    $pzmApiCtrlErr     = $_[1];
    $pzmApiCtrlVerbose = $_[2];

    chomp($ccmd);
    @cmd = split(/:/,$ccmd);
    logit($$pzmApiCtrlVerbose,LOG_DETAILS,BLUE,"# control command section -> $ccmd\n");
    switch (lc($cmd[0])){
      case "ver"          { logit(LOG_DETAILS,LOG_NONE,WHITE,$$env{VERSION}." \n");}
      case "input"        { CtrlCmdInput($ccmd);}
      case "config"       { CtrlCmdCfg($ccmd);}
      case "output"       { CtrlCmdOutput($ccmd);}
      case "system"       { CtrlCmdSystem($ccmd);}
      case "sandbox"      { CtrlCmdSandbox($ccmd);}
      case "log"          { CtrlCmdLog($ccmd);}
      # default
        else { logit(LOG_DETAILS,LOG_NONE,RED,"Unknonwn Control Command. EXITING.\n"); $$pzmApiCtrlErr+=1; }
    }
  }
}
# }}}

#
# **********************************************************
# {{{ PARSER METHODS
#

#
# METHOD :
# Control Command for INPUT section.
#
sub CtrlCmdInput{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
    my @cmd;
    chomp($_[0]);
    @cmd = split(/:/,$_[0]);
    if (defined $cmd[1])
      {
        logit($$pzmApiCtrlVerbose,LOG_DETAILS,BLUE,"# INPUT control command section -> $cmd[1]\n");
        switch (lc($cmd[1])){
          case "help"    { CtrlInputHelp($_[0]); }
          case "setup"   { CtrlInputSetup($_[0]); }
          case "import"  { CtrlInputImport($_[0]); }
          # default
          else { logit(LOG_DETAILS,LOG_NONE,RED,"Unknonwn INPUT Control Command. EXITING.\n"); $$pzmApiCtrlErr+=1; }
        }
      }
  }
}

#
# METHOD :
# Control Command for CONFIG vectors.
#
sub CtrlCmdCfg{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
  my @cmd;
  chomp($_[0]);
  @cmd = split(/:/,$_[0]);
  if (defined $cmd[1])
  {
    logit($$pzmApiCtrlVerbose,LOG_DETAILS,BLUE,"# CONFIG control command section -> $cmd[1]\n");
    switch (lc($cmd[1])){
      case "help"    { CtrlCfgHelp($_[0]); }
      case "setup"   { CtrlCfgSetup($_[0]); }
      case "import"  { CtrlCfgImport($_[0]); }
    # default
    else { logit(LOG_DETAILS,LOG_NONE,RED,"Unknonwn CONFIG Control Command. EXITING.\n"); $$pzmApiCtrlErr+=1; }
    }
  }
}
}

#
# METHOD :
# Control Command for OUTPUT section.
#
sub CtrlCmdOutput{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
  my @cmd;
  chomp($_[0]);
  @cmd = split(/:/,$_[0]);
  if (defined $cmd[1])
  {
    logit($$pzmApiCtrlVerbose,LOG_DETAILS,BLUE,"# OUTPUT control command section -> $cmd[1]\n");
    switch (lc($cmd[1])){
      case "help"    { CtrlOutputHelp($_[0]); }
      case "setup"   { CtrlOutputSetup($_[0]); }
      case "import"  { CtrlOutputImport($_[0]); }
      case "cleanup" { CtrlOutputCleanup($_[0]); }
      case "move"    { CtrlOutputMove($_[0]); }
    # default
    else { logit(LOG_DETAILS,LOG_NONE,RED,"Unknonwn OUTPUT Control Command. EXITING.\n"); $$pzmApiCtrlErr+=1; }
    }
  }
}
}

#
# METHOD :
# Control Command for SANDBOX section.
#
sub CtrlCmdSandbox{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
  my @cmd;
  chomp($_[0]);
  @cmd = split(/:/,$_[0]);
  if (defined $cmd[1])
  {
    logit($$pzmApiCtrlVerbose,LOG_DETAILS,BLUE,"# SANDBOX control command section -> $cmd[1]\n");
    switch (lc($cmd[1])){
      case "help"    { CtrlSandboxHelp($_[0]); }
      case "cleanup" { CtrlSandboxCleanup($_[0]); }
      case "move"    { CtrlSandboxMove($_[0]); }
    # default
    else { logit(LOG_DETAILS,LOG_NONE,RED,"Unknonwn SANDBOX Control Command. EXITING.\n"); $$pzmApiCtrlErr+=1; }
    }
  }
}
}

#
# METHOD :
# Control Command for SYSTEM section.
#
sub CtrlCmdSystem{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
  my @cmd;
  chomp($_[0]);
  @cmd = split(/:/,$_[0]);
  if (defined $cmd[1])
  {
    logit($$pzmApiCtrlVerbose,LOG_DETAILS,BLUE,"# SYSTEM control command section -> $cmd[1]\n");
    switch (lc($cmd[1])){
      case "help"    { CtrlSystemHelp($_[0]); }
      case "syscall" { CtrlSystemSyscall($_[0]); }
      case "setup"   { CtrlSystemSetup($_[0]); }
    # default
    else { logit(LOG_DETAILS,LOG_NONE,RED,"Unknonwn SYSTEM Command. EXITING.\n"); $$pzmApiCtrlErr+=1; }
    }
  }
}
}


#
# METHOD :
# Control Command for LOG section.
#
sub CtrlCmdLog{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
  my @cmd;
  chomp($_[0]);
  @cmd = split(/:/,$_[0]);
  if (defined $cmd[1])
  {
    logit($$pzmApiCtrlVerbose,LOG_DETAILS,BLUE,"# LOG control command section -> $cmd[1]\n");
    switch (lc($cmd[1])){
      case "help"    { CtrlLogHelp($_[0]); }
      case "cleanup" { CtrlLogCleanup($_[0]); }
      case "move"    { CtrlLogMove($_[0]); }
    # default
    else { logit(LOG_DETAILS,LOG_NONE,RED,"Unknonwn LOG Control Command. EXITING.\n"); $$pzmApiCtrlErr+=1; }
    }
  }
}
}



# }}}

#
# **********************************************************
# {{{ SUBROUTINES
#

#
# {{{ CopyRight
#
sub CopyRight(){
  print BOLD, BLUE,"\n\n#############################################\n";
  print "#\n";
  print "# Copyright <A9> 2011 Gianluca Filippini, Kevin McIntire\n";
  print "# All Rights Reserved\n";
  print "#\n";
  print "#############################################\n";
  print "\n\n",RESET;
}
# }}}


#
# {{{ INPUT SECTION
#

#
# input:help
#
sub CtrlInputHelp{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
    logit(LOG_DETAILS,LOG_NONE,WHITE,"INPUT CtrlInputHelp (t.b.implemented)\n");
  }
}

#
# input:setup
#
sub CtrlInputSetup{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
    logit(LOG_DETAILS,LOG_NONE,WHITE,"INPUT CtrlInputSetup (t.b.implemented)\n");
  }
}

#
# input:import
#
sub CtrlInputImport{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
    logit(LOG_DETAILS,LOG_NONE,WHITE,"INPUT CtrlInputImport (t.b.implemented)\n");
  }
}
# }}}


#
# {{{ CONFIG SECTION
#

#
# config:CtrlCfgHelp
#
sub CtrlCfgHelp{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
    logit(LOG_DETAILS,LOG_NONE,WHITE,"CONFIG CtrlCfgHelp (t.b.implemented)\n");
  }
}

#
# config:CtrlCfgSetup
#
sub CtrlCfgSetup{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
    logit(LOG_DETAILS,LOG_NONE,WHITE,"CONFIG CtrlCfgSetup (t.b.implemented)\n");
  }
}

#
# config:CtrlCfgImport
#
sub CtrlCfgImport{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
    logit(LOG_DETAILS,LOG_NONE,WHITE,"CONFIG CtrlCfgImport (t.b.implemented)\n");
  }
}
# }}}


#
# {{{ OUTPUT SECTION
#

#
# output:CtrlOutputHelp
#
sub CtrlOutputHelp{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
    logit(LOG_DETAILS,LOG_NONE,WHITE,"OUTPUT CtrlOutputHelp   (t.b.implemented)\n");
  }
}
#
# output:CtrlOutputSetup 
#
sub CtrlOutputSetup{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
    logit(LOG_DETAILS,LOG_NONE,WHITE,"OUTPUT CtrlOutputSetup  (t.b.implemented)\n");
  }
}
#
# output:CtrlOutputImport 
#
sub CtrlOutputImport{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
    logit(LOG_DETAILS,LOG_NONE,WHITE,"OUTPUT CtrlOutputImport  (t.b.implemented)\n");
  }
}
#
# output:CtrlOutputCleanup 
#
sub CtrlOutputCleanup{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
    logit(LOG_DETAILS,LOG_NONE,WHITE,"OUTPUT CtrlOutputCleanup  (t.b.implemented)\n");
  }
}
#
# output:CtrlOutputMove 
#
sub CtrlOutputMove{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
    logit(LOG_DETAILS,LOG_NONE,WHITE,"OUTPUT CtrlOutputMove  (t.b.implemented)\n");
  }
}
# }}}


#
# {{{ SYSTEM SECTION
#

#
# system:CtrlSystemHelp
#
sub CtrlSystemHelp{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
    logit(LOG_DETAILS,LOG_NONE,WHITE,"SYSTEM CtrlSystemHelp  (t.b.implemented)\n");
  }
}

#
# system:CtrlSystemSyscall
#
sub CtrlSystemSyscall{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
    logit(LOG_DETAILS,LOG_NONE,WHITE,"SYSTEM CtrlSystemSyscall  (t.b.implemented)\n");
  }
}

#
# system:CtrlSystemSetup
#
sub CtrlSystemSetup{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
    logit(LOG_DETAILS,LOG_NONE,WHITE,"SYSTEM CtrlSystemSetup (t.b.implemented)\n");
  }
}
# }}}


#
# {{{ SANDBOX SECTION
#

#
# sandbox:CtrlSandboxHelp
#
sub CtrlSandboxHelp{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
    logit(LOG_DETAILS,LOG_NONE,WHITE,"SANDBOX CtrlSandboxHelp (t.b.implemented)\n");
  }
}
#
# sandbox:CtrlSandboxCleanup
#
sub CtrlSandboxCleanup{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
    logit(LOG_DETAILS,LOG_NONE,WHITE,"SANDBOX CtrlSandboxCleanup (t.b.implemented)\n");
  }
}
#
# sandbox:CtrlSandboxMove
#
sub CtrlSandboxMove{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
    logit(LOG_DETAILS,LOG_NONE,WHITE,"SANDBOX CtrlSandboxMove (t.b.implemented)\n");
  }
}

# }}}

#
# {{{ LOG SECTION
#

#
# log:CtrlLogHelp
#
sub CtrlLogHelp{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
    logit(LOG_DETAILS,LOG_NONE,WHITE,"LOG CtrlLogHelp  (t.b.implemented)\n");
  }
}
#
# log:CtrlLogCleanup
#
sub CtrlLogCleanup{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
    logit(LOG_DETAILS,LOG_NONE,WHITE,"LOG CtrlLogCleanup  (t.b.implemented)\n");
  }
}
#
# log:CtrlLogMove
#
sub CtrlLogMove{
  if ($_[0] && (-1!=$pzmApiCtrlErr) && (1!=$pzmApiCtrlVerbose)){
    logit(LOG_DETAILS,LOG_NONE,WHITE,"LOG CtrlLogMove  (t.b.implemented)\n");
  }
}
# }}}



# }}}


##needed for "require" use
1;

__END__
