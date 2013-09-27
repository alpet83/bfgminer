#!/bin/sh

ping nl1.ghash.io -c 1
ping us1.ghash.io -c 1

DATE=`date +%F_%H-%M`
FREEMEM=`head /proc/meminfo | grep MemFree | awk '{ print $2 }'`
MINER='bfgminer'

if [ $FREEMEM  -lt 50000  ]; then
   echo 'Low freeMemory, trying kill $MINER ' >> /var/log/mining.log
   killall $MINER
   /sbin/mine.sh
fi

if [ `ps aux | grep $MINER | grep -v grep | wc -l` -eq 0 ]; then
    echo $DATE $MINER ' not found, restarting ' >> /var/log/mining.log
    /sbin/mine.sh;
else
    echo $DATE' $MINER active, freeMemory = '$FREEMEM >> /var/log/mining.log
fi

