#!/bin/bash
#
# Copyright 2011 Kevin McIntire, Gianluca Filippini
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
if [ $# -lt 2 ]
then
  echo "Usage: `basename $0` <dest> <files>"
  echo "  where <dest> is the destination dir in the repo"
  echo "        <files> is the basename of the files to commit"
  exit $E_BADARGS
fi

target=$1
shift
files=$*

versionFile=/mnt/input/aiv_cnt.txt
cur=`grep -v "^#" $versionFile`
version=`echo "$cur+1"|bc`
dir=`printf "%12.12d" $version`
destDir=/mnt/input/$target/$dir
mkdir -p $destDir
mv $files $destDir
tmpFile=/tmp/dickwad
sed -e s/$cur/$version/g $versionFile>$tmpFile
mv $tmpFile $versionFile

