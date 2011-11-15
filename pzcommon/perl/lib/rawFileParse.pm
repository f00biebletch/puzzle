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
package rawFileParse;

use strict;
use warnings;
use Term::ANSIColor qw(:constants);
use Exporter;
use JSON;

use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);

$VERSION     = 1.00;
@ISA         = qw(Exporter);
@EXPORT = qw( rawFileInfo rawFileNameParse );

sub rawFileNameParse
{
    my $ans = {};
    my @toks = split(/_/, substr($_[0], 0,(length($_[0])-4)));

    my $cur = 0;
    $ans->{FILENAME} = $_[0];
    $ans->{NAME} = $toks[$cur++];
    $ans->{RESOLUTION} = $toks[$cur++];    
    for (@toks[$cur..$#toks])
    {
        (my $k, my $v) = reverse(split(/x/));
        $ans->{$k} = $v;
    }
    return $ans;
}

# Function
# Purpose: 
# Params :  $_[0] is the raw file to check, fullpath
#
sub rawFileInfo{
  my $quiet=1;
  #
  my $params_valid;
  my $line;
  my $tmp;
  my @tmp;
  #
  my @descriptor;
  my %ans;
  my $err=0;

  if(defined $_[1]){
    if(!(lc($_[1]) eq "q")){
      $quiet = 1;
    }
  }

  if (defined $_[0])
    {
      my $infile=$_[0];

      # Check for input file type
      ###########################
      $tmp = substr($infile, -4);

      if( (lc($tmp) eq ".raw") || (lc($tmp) eq ".yuv") || (lc($tmp) eq ".txt"))
        {
          # filename buld.
          $tmp=substr($infile, 0, (length($infile) - 4) );
          $tmp=$tmp.".txt";

          # descriptor file exists?
          if( -f $infile)
            {
              ## read and store in a list the file content
              open(INFILE,"<$tmp");
              my(@lines) = <INFILE>;
              close(INFILE);

              ## loop on the list to build the command
              foreach $line (@lines)
                {
                  my $bkp=$line;
                  chomp($line);
                  if (($line =~ /^#/)
                      ||($line =~ /^%/)
                      ||($line =~ /^\/+/)
                      #||($line =~ /[\s+|\r+|\n+]{1,}$/)
                     )
                    {
                      ##print "[DBG] skipped : [$line]\n";
                    }
                  else{
                    $params_valid++;
                    push(@descriptor, $bkp);
                  }
                }
              #convert result to hash
              if($params_valid>0)
                {
                  my $string = join('',@descriptor);
                  #print $string;
                  $ans{VALUE}=from_json($string);
                }
              # sanity check : specification is correct
              if( (defined $ans{VALUE}{SPEC}) ){
                if ($ans{VALUE}{SPEC}{VERSION} eq "0.1"){
                  # mandatory sections check
                  if ( (!(defined $ans{VALUE}{CAMERA}))  ||
                       (!(defined $ans{VALUE}{PICTURE})) ||
                       (!(defined $ans{VALUE}{COLOR}))   ||
                       (!(defined $ans{VALUE}{EDITING})) ||
                       (!(defined $ans{VALUE}{PLAYBACK})) ){
                    $err+=1;
                    $ans{VALUE}{ERROR}{$err}="api ver 0.1 : missing mandatory section.";
                  }
                  # t.b.d : section value conformance
                  #
                  # t.b.d : cross section conformance
                  #
                  # t.b.d : see if the values declared in the filename are the ones in the descriptor
                  # 
                }
                else{
                  $err+=1;
                  $ans{VALUE}{ERROR}{$err}="api version not supported.";
                }
              }
              else{
                $err+=1;
                $ans{VALUE}{ERROR}{$err}="api spec section missing.";
              }
            }
        }
      else{
        $err+=1;
        $ans{VALUE}{ERROR}{$err}="descriptor file not found.";
      }
    }
  else
    {
      $err+=1;
      $ans{VALUE}{ERROR}{$err}="invalid input file.";
    }

  # error??
  if($err){
    $err+=1;
    $ans{VALUE}{ERROR}{$err}="parsing Failed";
    $ans{STATUS}="-1";
  }
  else{
    $ans{STATUS}="0";
  }

  # printout
  if(!($quiet))
    {
      my $jobj = JSON->new->utf8(1)->pretty(1)->encode(\%ans);
      
      print "\n$jobj\n";
    }

  ## return result
  return \%ans;
}

##needed for "require" use
1;

__END__
