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
package SysCheck;

use strict;
use warnings;
use Term::ANSIColor qw(:constants);
use Exporter;
use Scalar::Util 'reftype';
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

$VERSION     = 1.00;
@ISA         = qw(Exporter);
@EXPORT = qw( CheckOS CheckHW );


# CheckOS
# Purpose: compare $HostOS $NeededOS hash with all the
#          checks based on family types etc. etc.
# Params
# 1 : hash for the OS input type.
#     hash is expected to be in the form
#     OS => {BRAND=>"microsoft",
#            TYPE=>"windows",
#            VERSION=>{MAJOR=>"xp",MINOR=>"sp2"},
#            BIT => "32"}
#
# Return 1 if $HostOS is compatible with $NeededOS, 0 if not.
# Note: hash values are gather using external
#       system tools.
#
sub CheckOS{
  my $ans=0;
  my $reftype = reftype $_[0];

  if (defined $reftype)
    {
      if ( $reftype eq 'HASH' )
        {
          # put the detection code here
          # ...
          # print "fake CheckOS : $_[0] $_[0]{OS}{BIT}\n";
          # put the control code here
          # example : $ans=1 if ($_[0]{OS}{BIT} eq $detected_bit);
          $ans=1;
          # ...
        }
    }
  return $ans;
}


# CheckHW
# Purpose: compare $HostHW with %NeededHW hash with all the
#          checks based on intel types etc. etc.
# Params
# 1 : hash for the HW .
#     hash is expected to be in the form
#     HW => {ARCH =>"x86",
#            ISET =>"SSE2",
#            CPU=>{BRAND=>"intel",NAME=>"core2duo",CODENAME=>"conroe",MODEL=>"3040"},
#            CHECK=>"ge"},
#
# Return 1 if $HostHW is compatible with %NeededOS, 0 if not.
# Note: hash values are gather using external
#       system tools.
#
sub CheckHW{
  my $ans=0;
  my $reftype = reftype $_[0];

  if (defined $reftype)
    {
      if ( $reftype eq 'HASH' )
        {

          #put the detection code here
          # ...
          #print "fake CheckHW : $_[0] $_[0]{HW}{ARCH}\n";
          #print "fake CheckHW\n";
          #put the control code here
          # ...
          $ans=1;
        }
    }
  return $ans;
}


#
sub CheckResources{
  my $ans=0;
  my $reftype = reftype $_[0];

  if (defined $reftype)
    {
      if ( $reftype eq 'HASH' )
        {

          #put the detection code here
          # ...
          #put the control code here
          # ...
          $ans=1;
        }
    }
  return $ans;
}

# Detect OS type
# --------------
# see http://alma.ch/perl/perloses.htm for Perl OS list.
sub detectHostOS{
  my %Host=();

  if ("$^O" =~ /linux/i || (-d "/etc" && -d "/var" && "$^O" !~ /cygwin/i)) {
    $Host{OS}{TYPE}='linux';
  }
  if ("$^O" !~ /linux/i && -d "/etc" && -d "/Users") {
    $Host{OS}{TYPE}='macosx';
  }
  if ("$^O" =~ /cygwin/i || "$^O" =~ /win32/i) {
    $Host{OS}{TYPE}='win32';
  }
  return \%Host;
}




##needed for "require" use
1;

__END__
