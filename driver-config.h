#ifndef __DRIVER_CONFIG_H__
#define __DRIVER_CONFIG_H__

#include "btypes.h"

// here controlled stats and performance driver-bitfury

#define BITFURY_ENABLE_LONG_STAT 1
#define BITFURY_ENABLE_SHORT_STAT 1
// #define BITFURY_AUTOCLOCK
#define BITFURY_SCANHASH_DELAY 30
#define BFGMINER_MOD
// #define CGMINER_MOD

// #define FAST_CLOCK1

// processing all chips every cycle (remains about 500ms)
// #define BITFURY_HARD_LOAD
// #define USE_LIVE_ORDER
#define WORK_FRAME 200
// #define DOUBLE_TEST

// print by chip stats in log files in /var/log/bitfury
// #define BITFURY_CHIP_STAT

#ifdef BFGMINER_MOD
#define nmsleep cgsleep_ms
#define nusleep cgsleep_us

void format_time(timeval_p tv, char *datetime);
#endif

#ifdef FAST_CLOCK1
        #define BASE_OSC_BITS 51
        #define LOW_HASHRATE 2.2
#else
        #define BASE_OSC_BITS 53
        #define LOW_HASHRATE 1.5
#endif


#endif
