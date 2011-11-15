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
package JSONLog;

use strict;
use warnings;
use Exporter;
use JSON;
use Scalar::Util 'reftype';
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

$VERSION     = 1.00;
@ISA         = qw(Exporter);
@EXPORT = qw( JSONLog JSONLogErr JSONLogStatus);


# JSON Log Function
# Purpose: 
# print in plain text if 1st param is "jTXT"
sub JSONLog{
    my $reftype;

    if ($_[0] eq "jTXT")
    {
        $reftype = reftype $_[1];
        if (defined $reftype)
        {
          if ( $reftype eq 'SCALAR' ) {
            print "JLog:SCALAR: $_[1]\n";
          }
          else{
            my $jobj = JSON->new->utf8(1)->pretty(1)->encode($_[1]);
	    #$jobj=lc($jobj); #reduce all to lowercase
            print "$jobj\n";
	  }	
	}
	else{
            print "$_[1]\n";
	}

    }
    elsif ($_[0] eq "jJSON")
    {
        $reftype = reftype $_[1];
        if (defined $reftype)
        {
          if ( $reftype eq 'SCALAR' ) {
            print "JLog:SCALAR: $_[1]\n";
          }
          else{
            my $jobj = JSON->new->utf8(1)->pretty(0)->encode($_[1]);
	    #$jobj=lc($jobj); #reduce all to lowercase
	    print "$jobj\n";
	  }	
	}
	else{
            print "$_[1]\n";
	}
    }
    else
    {
        $reftype = reftype $_[0];
        if (defined $reftype)
        {
          if ( $reftype eq 'SCALAR' ) {
            print "JLog:SCALAR: $_[0]\n";
          }
          else{
            my $jobj = JSON->new->utf8(1)->pretty(1)->encode($_[0]);
	    #$jobj=lc($jobj); #reduce all to lowercase
	    print "$jobj\n";
	  }	
	}
	else{
            print "$_[0]\n";
	}
    }
}



##needed for "require" use
1;

__END__

