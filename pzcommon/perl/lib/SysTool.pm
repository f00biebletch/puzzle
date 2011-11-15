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
package SysTool;

use strict;
use warnings;
use Term::ANSIColor qw(:constants);
use Exporter;
use Scalar::Util 'reftype';
use Switch;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS);
use JSON;
use FindBin qw($Bin);
$VERSION     = 1.00;
@ISA         = qw(Exporter);
@EXPORT = qw( getActionOpt getActionCmd getDateTimeNow logFileDump logFileParse parsePzmConfigIndex parsePzmConfigMap parsePzmConfigMapInputList );




# {{{ parsePzmConfigMap{
# Purpose : return the whole confing.map structure (json obj => hash table)
#
# Params :
# $_[0]: data struct to fill
# $_[1]: path to the config file
#
sub parsePzmConfigMap{
  my $parse_err=0;
  my $valid_line=0;
  my @descriptor;
  my $result;
  my $ans;
  my $line;
  my $datakey;
  my $filekey;
  #
  #

  if ( (defined $_[1]) && (defined $_[1]) ){
    # paranoia : check the input types for $_[0]
    # must be an array.
    my $intype = reftype $_[0];
    if (defined $intype){
      if (!( $intype eq 'HASH' )){
        $parse_err+=1;
      }
    }
    else{$parse_err+=1;}
    #
    if ( (!($parse_err))&&(-d $_[1]) ){
      my $indexFile=$_[1]."/config.map";
      if(-f $indexFile){
        #file exists, parse it
        ## read and store in a list the file content
        open(INFILE,"< $indexFile");
        my(@lines) = <INFILE>;
        close(INFILE);
        ## loop on the list
        foreach $line (@lines)
          {
            my $bkp=$line;
            chomp($line);
            if (($line =~ /^#/)
                ||($line =~ /^%/)
                ||($line =~ /^\/+/)
                ||($line =~ /[\s+|\r+|\n+]{1,}$/)
               )
              {
               ##DBG##  print "[DBG] skipped : [$line]\n";
              }
            else{
              $valid_line++;
              push(@descriptor, $bkp);
            }
          }
        #convert result to hash
        if($valid_line>0){
          my $string = join('',@descriptor);
          $result = from_json($string);
        }
        else{$parse_err+=1;}

        ## result has been formed
        ## check if config vectors exists and add them
        my %ans;
        if (!($parse_err)){
          for $datakey ( keys %{$result} ){
            if ( ( exists ($result->{$datakey}{DIR}) ) &&
                 ( exists ($result->{$datakey}{FILE}) ) &&
                 ( exists ($result->{$datakey}{SELECT_PL}) ) &&
                 ( exists ($result->{$datakey}{SELECT_CFG}) ) ){
              for $filekey ( @{$result->{$datakey}{FILE}} ){
                my $InputFile           = $_[1]."../input/".$result->{$datakey}{DIR}."/".$filekey.".raw";
                my $InputDescriptorFile = $_[1]."../input/".$result->{$datakey}{DIR}."/".$filekey.".txt";
                if ( (-f $InputFile) && (-f $InputDescriptorFile) ){
                  my $inFilename = $filekey.".raw";
                  ## dirty trick : I use a hash to get unique vale insertion!!
                  $ans{$inFilename}="1";
                }
                else{$parse_err+=1;}
              }
            } # if exists
            else{$parse_err+=1;}
          }
        }
        ## if no errors we override the $_[0] (the data struct)
        if (!($parse_err)){
          # put data to the input struct
          %{$_[0]} = %{$result};
        }
      }
      else{$parse_err+=1;}
    }
    else{$parse_err+=1;}
  }
  else{$parse_err+=1;}
  #
  ##DBG## print "[TRACE] parsePzmConfigIndex []exit\n";
  return $parse_err;
}
# }}}


# {{{ parsePzmConfigMapInputList{
# Purpose : return the list of source (raw) file needed
#
# Params :
# $_[0]: data struct to fill
# $_[1]: path to the config file
#
sub parsePzmConfigMapInputList{
  my $parse_err=0;
  my $valid_line=0;
  my @descriptor;
  my $result;
  my @ans;
  my $line;
  my $datakey;
  my $filekey;
  #
  #

  if ( (defined $_[1]) && (defined $_[1]) ){
    # paranoia : check the input types for $_[0]
    # must be an array.
    my $intype = reftype $_[0];
    if (defined $intype){
      if (!( $intype eq 'ARRAY' )){
        $parse_err+=1;
      }
    }
    else{$parse_err+=1;}
    #
    if ( (!($parse_err))&&(-d $_[1]) ){
      my $indexFile=$_[1]."/config.map";
      if(-f $indexFile){
        #file exists, parse it
        ## read and store in a list the file content
        open(INFILE,"< $indexFile");
        my(@lines) = <INFILE>;
        close(INFILE);
        ## loop on the list
        foreach $line (@lines)
          {
            my $bkp=$line;
            chomp($line);
            if (($line =~ /^#/)
                ||($line =~ /^%/)
                ||($line =~ /^\/+/)
                ||($line =~ /[\s+|\r+|\n+]{1,}$/)
               )
              {
               ##DBG##  print "[DBG] skipped : [$line]\n";
              }
            else{
              $valid_line++;
              push(@descriptor, $bkp);
            }
          }
        #convert result to hash
        if($valid_line>0){
          my $string = join('',@descriptor);
          $result = from_json($string);
        }
        else{$parse_err+=1;}

        ## result has been formed
        ## check if config vectors exists and add them
        my %ans;
        if (!($parse_err)){
          for $datakey ( keys %{$result} ){
            if (( exists ($result->{$datakey}{DIR}) ) && ( exists ($result->{$datakey}{FILE}) ) ){
              for $filekey ( @{$result->{$datakey}{FILE}} ){
                my $InputFile           = $_[1]."../input/".$result->{$datakey}{DIR}."/".$filekey.".raw";
                my $InputDescriptorFile = $_[1]."../input/".$result->{$datakey}{DIR}."/".$filekey.".txt";
                if ( (-f $InputFile) && (-f $InputDescriptorFile) ){
                  my $inFilename = $filekey.".raw";
                  ## dirty trick : I use a hash to get unique vale insertion!!
                  $ans{$inFilename}="1";
                }
                else{$parse_err+=1;}
              }
            } # if exists
            else{$parse_err+=1;}
          }
        }
        ## if no errors we override the $_[0] (the data struct)
        if (!($parse_err)){
          @ans=();
          for $datakey ( keys %ans ){
            push (@ans, $datakey );
          }
          # put data to the input struct
          @{$_[0]} = @ans;
        }
      }
      else{$parse_err+=1;}
    }
    else{$parse_err+=1;}
  }
  else{$parse_err+=1;}
  #
  ##DBG## print "[TRACE] parsePzmConfigIndex []exit\n";
  return $parse_err;
}
# }}}


# {{{ parsePzmConfigIndex{
# Purpose
#
# Params :
# $_[0]: data struct to fill
# $_[1]: path to the config file
#
sub parsePzmConfigIndex{
  my $parse_err=0;
  my $valid_line=0;
  my @descriptor;
  my $result;
  my @ans;
  my $line;
  my $datakey;
  #
  #

  if ( (defined $_[1]) && (defined $_[1]) ){
    # paranoia : check the input types for $_[0]
    # must be an array.
    my $intype = reftype $_[0];
    if (defined $intype){
      if (!( $intype eq 'ARRAY' )){
        $parse_err+=1;
      }
    }
    else{$parse_err+=1;}
    #
    if ( (!($parse_err))&&(-d $_[1]) ){
      my $indexFile=$_[1]."/index.txt";
      if(-f $indexFile){
        #file exists, parse it
        ## read and store in a list the file content
        open(INFILE,"< $indexFile");
        my(@lines) = <INFILE>;
        close(INFILE);
        ## loop on the list
        foreach $line (@lines)
          {
            my $bkp=$line;
            chomp($line);
            if (($line =~ /^#/)
                ||($line =~ /^%/)
                ||($line =~ /^\/+/)
                ||($line =~ /[\s+|\r+|\n+]{1,}$/)
               )
              {
               ##  print "[DBG] skipped : [$line]\n";
              }
            else{
              $valid_line++;
              push(@descriptor, $bkp);
            }
          }
        #convert result to hash
        if($valid_line>0){
          my $string = join('',@descriptor);
          $result = from_json($string);
        }
        else{$parse_err+=1;}

        ## result has been formed
        ## check if config vectors exists and add them
        if (!($parse_err)){
          for $datakey ( keys %{$result} ){
            my $vectorCfgFile = $_[1]."/".$datakey.".txt";
            if (-f $vectorCfgFile){
              push (@ans, $datakey.".txt" );
            }
            else{$parse_err+=1;}
          }
        }
        ## if no errors we override the $_[0] (the data struct)
        if (!($parse_err)){
          @{$_[0]} = @ans;
        }
      }
      else{$parse_err+=1;}
    }
    else{$parse_err+=1;}
  }
  else{$parse_err+=1;}
  #
  ##DBG## print "[TRACE] parsePzmConfigIndex []exit\n";
  return $parse_err;
}
# }}}


# {{{ getActionOpt from syntax "act:cmd:opt"
# Purpose: compare $HostOS $NeededOS hash with all the
#          checks based on family types etc. etc.
# Params : one imput array
#
sub getActionOpt{
  my $opt="";
  if (defined $_[2]){
    switch (lc($_[2]))
      {
        case "quiet" {$opt="q";}
        case "q"     {$opt="q";}
        else {
          # option is not known, nothing to do
          $opt = $_[2];
        }
      }
  }
  return $opt;
}
# }}}


# {{{ getActionOpt from syntax "act:cmd:opt"
# Purpose: compare $HostOS $NeededOS hash with all the
#          checks based on family types etc. etc.
# Params : one imput array
#
sub getActionCmd{
  my $cmd="";
  if (defined $_[1]){
    $cmd=$_[1];
  }
  return $cmd;
}
# }}}


# {{{ addErrorEvent
#
sub addErrorEvent{
##t.b.d
}
# }}}


# {{{ getDateTimeNow()
#
#
sub getDateTimeNow(){
  my $sec; my $min; my $hour; my $mday; my $mon; my $year;
  ($sec,$min,$hour,$mday,$mon,$year)=localtime(time);
  my $tmp = ($year+1900)."/".($mon+1)."/".$mday." ".$hour.":".$min.".".$sec;
  ##
  return $tmp
}
# }}}


# {{{ logFileDump
#
#
sub logFileDump{
  if(-f $_[0]){
    open(LOGFILE, ">>$_[0]");
    print LOGFILE $_[1]."\n";
    close(LOGFILE);
    return 0;
  }
  else{
    return -1;
  }
}
# }}}


# {{{ lofFileParse
# Purpose : parse the logfile to match a string at the
#           beginning of a line
# Params  : $_[0] is the input logfile
#           $_[1] is the string to parse
#
sub logFileParse{
  my $keyfound=0;
  my $line;
  my $err=0;

  if(-f $_[0]){
    open(LOGFILE, "<$_[0]");
    my(@lines) = <LOGFILE>;
    close(LOGFILE);

    for $line (@lines){
      chomp($line);
      ##print "parse: $line key:$_[1]\n";
      if($line=~/^$_[1]/i){
        $keyfound+=1;
      }
    }
  }
  else{
    $err+=1;
  }

  # everything ok?
  if(!($err)){
    ##print "KEYFOUND : ".$keyfound."\n";
    return $keyfound;
  }
  else{
    return -1;
  }
}
# }}}



sub fillinvec {
  	my $tstabspath = $Bin."/"; #.$ENV{TSTRELPATH}."/";
	opendir RAWS, $tstabspath."/../../input/obj_quality/";
	my @inputlist;
	my @rawfiles = readdir(RAWS);
	closedir(RAWS);
	for my $infile (@rawfiles) {
		next if $infile eq "." or $infile eq ".." or $infile eq ".svn";
		if (substr($infile, -4) eq ".raw") {
			push(@inputlist, $infile);
			}
		}
	return @inputlist;
}

sub getSeqInfo {
	my $tstabspath = $Bin."/";
	my @cliphashref;
	my $inclip = shift(@_);
	my $infoRawInput = rawFileParse::rawFileInfo($tstabspath."/../../input/obj_quality/".$inclip);

                      if(defined $infoRawInput->{STATUS}){
                        if(lc($infoRawInput->{STATUS}) eq "0"){
                          if( $infoRawInput->{VALUE}{SPEC}{RAWFILE} eq $inclip ){
                            #
                            my $inWidth = $infoRawInput->{VALUE}{PICTURE}{WIDTH};
                            my $inHeight = $infoRawInput->{VALUE}{PICTURE}{HEIGHT};
                            my $frRate = $infoRawInput->{VALUE}{PLAYBACK}{FRAMERATE};
				my %cres = (
						width => $inWidth,
						height => $inHeight,
						framerate => $frRate,
						);
				push(@cliphashref, \%cres);
					}
					}
					}
	return @cliphashref;

	}


##needed for "require" use
1;

__END__
