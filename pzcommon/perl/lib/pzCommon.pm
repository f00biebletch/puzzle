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
package pzCommShared;

use strict;
use warnings;
use Term::ANSIColor qw(:constants);
use Exporter;
use Scalar::Util 'reftype';
use SimpleLog;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
use JSON;
use FindBin qw($Bin);
$VERSION     = 1.00;
@ISA         = qw(Exporter);
@EXPORT = qw( getActionOpt getActionCmd getDateTimeNow logFileDump logFileParse parsePzmConfigIndex parsePzmConfigMap parsePzmConfigMapInputList );


=head1 NAME

pzCommon - Common subs called from jobs and TPTs in Puzzle.

=head1 SYNOPSIS

maintain these subs in one place to facilitate maint in the future.

=head1 DESCRIPTION

pzCommon provides a base implementation for the lifecycle
methods of a job.

=head1 METHODS

=head2 CopyRight()

Display the CopyRight info.

=over 4

=item $cmd

The command from the shell.

=back

=head3 Return

the CopyRight message.

=cut


#
# SUBROUTINE :
# {{{ TestJob CopyRight

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


=head2 worklist()

Base implementation of the Version method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=back

=head3 Return

return the Version info to be logged.

=cut

#
# METHOD :
# {{{ Control Command to query version number.
#
sub Version{
	my %INFO = $_[0];
  my @cmd;          # command input string
  my %ans=();       # anser hash, empry by default
  my $opt="";       # input option for the command
  my $quiet=0;      # FLAG : do we need to run quiet?

  logit(LOG_DETAILS,LOG_NONE,WHITE,"$INFO{\"VERSION\"}\n");
}
# }}}

=head2 CtrlCmdInput()

Base implementation of the CtrlCmdInput method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=back

=head3 Return

return the Standard JSON reply.

=cut

#
# METHOD :
# {{{ Control Command for INPUT vectors.
#
sub CtrlCmdInput{
	my %INFO = $_[0];
	my $err = 0;
	my $verbose = LOG_MIN;
  my @cmd;          # command input string
  my %ans=();       # anser hash, empry by default
  my $opt="";       # input option for the command
  my $quiet=0;      # FLAG : do we need to run quiet?
  #
  my %result;

  # sanity check
  if ($INFO{TYPE} eq "virtual"){
    # virtual TPT cannot control input
    $err+=1;
    JSONLogErr("virtual TPT, cannot control INPUT section.");
  }
  else{
    chomp($_[0]);
    @cmd = split(/:/,$_[0]);
    if (defined $cmd[1])
      {
        logit($verbose,LOG_DETAILS,BLUE,"# INPUT control command section -> $cmd[1]\n");
        if (lc($cmd[1]) eq "list"){
          	CtrlInList($_[0]);
			}
		elsif (lc($cmd[1]) eq "list"){
          	CtrlInInfo($_[0]);
			}
          # default
         else{
            logit(LOG_DETAILS,LOG_NONE,RED,"Unknonwn Control Command. EXITING.\n");
            $err+=1;
             }
        }
      }
  return $err;
}
# }}}

=head2 CtrlInList()

Base implementation of the CtrlInList method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=back

=head3 Return

return the Standard JSON reply.

=cut

#
# SUBROUTINE :
# {{{ sandbox : clean
#
sub CtrlInList{
 logit(LOG_DETAILS,LOG_NONE,WHITE,"INPUT LIST  (t.b.implemented)\n");
}
# }}}

=head2 CtrlInInfo()

Base implementation of the CtrlInInfo method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=back

=head3 Return

return the Standard JSON reply.

=cut

#
# SUBROUTINE :
# {{{ sandbox : list
#
sub CtrlInInfo{
  logit(LOG_DETAILS,LOG_NONE,WHITE,"INPUT INFO(t.b.implemented)\n");
}
# }}}

=head2 CtrlCmdCfg()

Base implementation of the CtrlCmdCfg method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=back

=head3 Return

return the Standard JSON reply.

=cut

#
# METHOD :
# {{{ Control Command for CONFIG vectors.
#
sub CtrlCmdCfg{
	my %INFO = $_[0];
	my $err = 0;	
	my $verbose = LOG_MIN;
  my @cmd;          # command input string
  my %ans=();       # anser hash, empry by default
  my $opt="";       # input option for the command
  my $quiet=0;      # FLAG : do we need to run quiet?
  #
  my %result;

  # sanity check
  if ($INFO{TYPE} eq "virtual"){
    # virtual TPT cannot control input
    $err+=1;
    JSONLogErr("virtual TPT, cannot control CONFIG section.");
  }
  else{
    chomp($_[0]);
    @cmd = split(/:/,$_[0]);
    if (defined $cmd[1])
      {
        logit($verbose,LOG_DETAILS,BLUE,"# CONFIG control command section -> $cmd[1]\n");
        if (lc($cmd[1]) eq "list"){
          		CtrlCfgList($_[0]);
				}
		elsif (lc($cmd[1]) eq "info"){
          		CtrlCfgInfo($_[0]);
				}
          # default
          else { logit(LOG_DETAILS,LOG_NONE,RED,"Unknonwn Control Command. EXITING.\n");
				$err+=1; }
        }
      }
  return $err;
}
# }}}

=head2 CtrlCfgList()

Base implementation of the CtrlCfgList method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=back

=head3 Return

return the Standard JSON reply.

=cut

#
# SUBROUTINE :
# {{{ sandbox : clean
#
sub CtrlCfgList{
  logit(LOG_DETAILS,LOG_NONE,WHITE,"CFG LIST  (t.b.implemented)\n");
}
# }}}

=head2 CtrlCfgInfo()

Base implementation of the CtrlCfgInfo method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=back

=head3 Return

return the Standard JSON reply.

=cut

#
# SUBROUTINE :
# {{{ sandbox : list
#
sub CtrlCfgInfo{
  logit(LOG_DETAILS,LOG_NONE,WHITE,"CFG DETAIL(t.b.implemented)\n");
}
# }}}

=head2 CtrlCmdSbox()

Base implementation of the CtrlCmdSbox method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=back

=head3 Return

return the Standard JSON reply.

=cut

#
# METHOD :
# {{{ Control Command for SANDBOX.
#
sub CtrlCmdSbox{
	my %INFO = $_[0];
	my $err = 0;
	my $verbose = LOG_MIN;
  my @cmd;          # command input string
  my %ans=();       # anser hash, empry by default
  my $opt="";       # input option for the command
  my $quiet=0;      # FLAG : do we need to run quiet?
  #
  my %result;
  #
  chomp($_[0]);

  # sanity check
  if ($INFO{TYPE} eq "virtual"){
    # virtual TPT cannot control input
    $err+=1;
    JSONLogErr("virtual TPT, cannot control SANDBOX section");
  }
  else{
    @cmd = split(/:/,$_[0]);
    if (defined $cmd[1]) {
        logit($verbose,LOG_DETAILS,BLUE,"# CONFIG control command section -> $cmd[1]\n");
        if (lc($cmd[1]) eq "list"){
          	CtrlSboxClean($_[0]);
			}
		elsif (lc($cmd[1]) eq "clean"){
          	CtrlSboxList($_[0]);
			}
            # default
        else { logit(LOG_DETAILS,LOG_NONE,RED,"Unknown Control Command. EXITING.\n");
				$err+=1; }
        }
      }
 return $err; 
}
# }}}

=head2 CtrlSboxClean()

Base implementation of the CtrlSboxClean method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=back

=head3 Return

return the Standard JSON reply.

=cut


#
# SUBROUTINE :
# {{{ sandbox : clean
#
sub CtrlSboxClean{
  logit(LOG_DETAILS,LOG_NONE,WHITE,"SBOX Clean  (t.b.implemented)\n");
}
# }}}

=head2 CtrlSboxList()

Base implementation of the CtrlSboxList method.

=head3 Arguments

=over 4

=item $cmd

The command from the shell.

=back

=head3 Return

return the Standard JSON reply.

=cut


#
# SUBROUTINE :
# {{{ sandbox : list
#
sub CtrlSboxList{
  logit(LOG_DETAILS,LOG_NONE,WHITE,"SBOX List (t.b.implemented)\n");
}
# }}}

1;
