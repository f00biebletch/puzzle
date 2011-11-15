#
# Copyright (c) 2011 Kevin McIntire, Gianluca Filippini
# All Rights Reserved
#
# Posts a heartbeat from a compute node to Puzzle..
#
# $Id$
#
ADDR=http://puzzle.net:8080/heartbeat

# Basically this mirrors hostinfo BUT it's stuff that
# is either needed regardless of node up/down OR
# stuff that cannot be derived
# TODO replace the values below with relevant machine info
DATA='
{
  "HOST" : {
    "NAME": "puzzle.grid.net",
    "IP":"127.0.0.1"
  },
  "HW": {
    "ARCH" : "x86",
    "CPU" : {
      "NAME" : "atom",
      "BRAND" : "intel",
      "MODEL" : "palomino",
      "CODENAME" : "dualdiamondville"
    }
  },
  "OS" : {
    "BIT" : "32",
    "BRAND" : "linux",
    "VERSION" : {
      "MINOR" : "10",
      "MAJOR" : "9"
    },
    "TYPE" : "ubuntu"
  },
  "max_concurrent_jobs" : 1
}
'

curl -d "$DATA" ${ADDR}
