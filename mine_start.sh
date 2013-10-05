#!/bin/sh
cd /home/pi/bfgminer
nice --20 /home/pi/bfgminer/bfgminer  -S metabank:auto --quiet-work-updates -T -c /home/pi/.cgminer/cgminer.conf --queue 200

