#!/bin/bash -x

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

if [ $# -ne 1 ]
then
    exit -1
fi

NODE_NAME=$1
TARGET_DIR=/usr/local/lib/pzd
ERL_DIR=/opt/local/lib/erlang
BASE_DIR=/Users/kevinmcintire/etc/qa/apps/puzzle/pzd/
RELEASE_FILE=${BASE_DIR}/rel/pzd_rel-1.tar.gz

USERNAME=kevinmcintire
GROUPNAME=staff

sudo mkdir -p ${TARGET_DIR}
sudo mkdir -p ${TARGET_DIR}/bin
sudo mkdir -p ${TARGET_DIR}/log
sudo mkdir -p ${TARGET_DIR}/pipe
sudo mkdir -p ${TARGET_DIR}/ndexe

sudo tar xzf ${RELEASE_FILE} -C ${TARGET_DIR}

TMP_FILE=/tmp/pzdscratch
sed -e "s/%%NODE_NAME%%/${NODE_NAME}/g" ${BASE_DIR}/etc/start > ${TMP_FILE}
sudo cp ${TMP_FILE} ${TARGET_DIR}/bin/start
sudo chmod +x ${TARGET_DIR}/bin/start
rm -f ${TMP_FILE}
sudo sed -e "s/%%NODE_NAME%%/${NODE_NAME}/g" ${BASE_DIR}/etc/stop > ${TMP_FILE}
sudo cp ${TMP_FILE} ${TARGET_DIR}/bin/stop
sudo chmod +x ${TARGET_DIR}/bin/stop
rm -f ${TMP_FILE}
sudo sed -e "s/%%NODE_NAME%%/${NODE_NAME}/g" ${BASE_DIR}/etc/reload > ${TMP_FILE}
sudo cp ${TMP_FILE} ${TARGET_DIR}/bin/reload
rm -f ${TMP_FILE}
sudo chmod +x ${TARGET_DIR}/bin/reload
sudo cp ${BASE_DIR}/etc/erl ${TARGET_DIR}/bin


sudo cp ${BASE_DIR}/etc/start_erl.data ${TARGET_DIR}/releases
sudo cp ${BASE_DIR}/etc/RELEASES ${TARGET_DIR}/releases

sudo cp $ERL_DIR/bin/run_erl ${TARGET_DIR}/bin
sudo cp $ERL_DIR/bin/start_erl ${TARGET_DIR}/bin
sudo cp $ERL_DIR/bin/start_sasl.boot ${TARGET_DIR}/bin/start.boot
sudo cp $ERL_DIR/bin/to_erl ${TARGET_DIR}/bin

# FIXIT install /etc/init.d?

# FIXIT deploy ndexe bootstrap? yeah!  call ../ndexe/etc/bootstrap.sh

sudo chown -R ${USERNAME}:${GROUPNAME} ${TARGET_DIR}







