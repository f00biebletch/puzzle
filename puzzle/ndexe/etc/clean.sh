#!/bin/bash
# run this daily via cron 0 5 * * * cmd

find /mnt/output/pzmodules -maxdepth 2 -type d -name '*20[0-9][0-9]*' -mtime +10 -exec rm -rf {} \;
find /mnt/input/pzmodules -maxdepth 2 -type d -name '*20[0-9][0-9]*' -mtime +10 -exec rm -rf {} \;
