#ifndef __DRIVER_CONFIG_H__
#define __DRIVER_CONFIG_H__

// здесь контролируется производительность и статистика driver-bitfury

#define BITFURY_ENABLE_LONG_STAT 1
#define BITFURY_ENABLE_SHORT_STAT 1
// #define BITFURY_AUTOCLOCK
#define BITFURY_SCANHASH_DELAY 30
#define BFGMINER_MOD
// #define CGMINER_MOD

// #define FAST_CLOCK1
#define BITFURY_HARD_LOAD
#define BITFURY_MONITORING

#ifdef BFGMINER_MOD
#define nmsleep cgsleep_ms
#endif

#ifdef FAST_CLOCK1
        #define BASE_OSC_BITS 51
        #define LOW_HASHRATE 2.5
#else
        #define BASE_OSC_BITS 53
        #define LOW_HASHRATE 1.8
#endif

#endif
