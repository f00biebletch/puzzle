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
package RawParse;

use strict;
use warnings;
use Term::ANSIColor qw(:constants);
use Exporter;

use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

$VERSION     = 1.00;
@ISA         = qw(Exporter);
@EXPORT = qw( LOG_NONE LOG_MIN LOG_SIMPLE LOG_DETAILS LOG_MAX logit );

use constant {
    LOG_NONE  	=> 30,
    LOG_MIN  	=> 40,
    LOG_SIMPLE 	=> 50,
    LOG_DETAILS => 60,
    LOG_MAX   	=> 90,
};

#require Exporter;
#@ISA    = qw( Exporter );


# Simple Log Function
# Purpose: log on the screen for color/verbosity level
# Params Num: 4 
#   0 : current verbosity
#   1 : verbosity threshold
#   2 : color
#   3 : message
sub logit{
  if ($_[0]>=$_[1]){
	if  ("" eq $_[2]){
	    print RESET;
    	print "$_[3]";		
    }
    else{
    	print BOLD,$_[2],"$_[3]",RESET;
    }
  }
}

##needed for "require" use
1;

__END__
