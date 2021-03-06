/*
 * device-bitfury.c - device functions for Bitfury chip/board library
 *
 * Copyright (c) 2013 bitfury
 * Copyright (c) 2013 legkodymov
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
*/


#include "miner.h"
#include <math.h>
#include <unistd.h>
#include <sha2.h>
#include "libbitfury.h"
#include "util.h"
#include "tm_i2c.h"
#include <stdio.h>
#include <execinfo.h>
#include <signal.h>
// #include <curses.h>
#include "uthash.h"
#include <malloc.h>
#include "driver-config.h"
#include <sys/stat.h>
// #include "memutil.h"

#define GOLDEN_BACKLOG 5
#define STAT_LINE_LENGTH 1024

#ifdef BITFURY_METABANK
 // print vCore for slots
 // #define BITFURY_MONITORING
#endif



typedef struct thr_info thr_info_t;
typedef struct timeval timeval_t;
typedef struct cgpu_info cgpu_info_t;
typedef timeval_t *timeval_p;

struct device_drv bitfury_drv;

// TODO: все следующие переменные надо переместить в структуру контекста устройства (!), которую адресовать из cgpu_info

const int short_stat = 20;

unsigned loops_count = 0;
unsigned call_count = 0;
static int last_chip = 0; // для кольцевого обхода по выдаче заданий
short chips_by_rate[50];
static int no_work = 0;
static double vc0_median[BITFURY_MAXBANKS];
static double vc1_median[BITFURY_MAXBANKS];
static double ghs_median[BITFURY_MAXBANKS];
char stat_lines[BITFURY_MAXBANKS][STAT_LINE_LENGTH] = {0};
static char CL_RESET[]     = "\e[0m";
static char CL_LT_RED[]    = "\e[1;31m";
static char CL_LT_GREEN[]  = "\e[1;32m";
static char CL_LT_YELLOW[] = "\e[1;33m";
static char CL_LT_BLUE[]   = "\e[1;34m";
static char CL_LT_CYAN[]   = "\e[1;36m";
static char CL_LT_WHITE[]  = "\e[1;37m";
static struct tm g_time;


int shares_first, shares_last, shares_total;
int stat_dumps = 0;
int slot_base_bits [BITFURY_MAXBANKS] = { BASE_OSC_BITS };


// Forward declarations
static void bitfury_disable(thr_info_t* thr);
static bool bitfury_prepare(thr_info_t *thr);
int calc_stat(time_t * stat_ts, time_t stat, struct timeval now);
int calc_stat_f(double * stat_ts, double elapse, double now_mcs);
double shares_to_ghashes(int shares, double seconds);

inline double tv2mcs(struct timeval *tv) {
    return (double)tv->tv_sec * 1e6 + (double)tv->tv_usec;
}


timeval_p get_cgtime() {
    static timeval_t now;
    cgtime(&now);
    memcpy (&g_time, localtime(&now.tv_sec), sizeof(g_time));
    return &now;
}


#ifdef BFGMINER_MOD
// alternative form for get_datestamp
// duplicated in logging.c
void format_time(timeval_p tv, char *datetime) {
    struct tm *tm;

    if (NULL == tv) {
        static timeval_t now;
        bfg_init_time();
        bfg_gettimeofday(&now);
        tv = &now;
    }


    const time_t tmp_time = tv->tv_sec;
    tm = localtime(&tmp_time);

    sprintf(datetime, " [%d-%02d-%02d %02d:%02d:%02d.%03d] ",
        tm->tm_year + 1900,
        tm->tm_mon + 1,
        tm->tm_mday,
        tm->tm_hour,
        tm->tm_min,
        tm->tm_sec,
        tv->tv_usec / 1000);
}
#endif


void sig_handler(int sig) {
  void *array[10];
  size_t size;

  // get void*'s for all entries on the stack
  size = backtrace(array, 10);

  // print out all the frames to stderr
  printf("Error: signal %d, trace-size %d: \n", sig, size);
  backtrace_symbols_fd(array, size, STDOUT_FILENO);
  exit(1);
}

static void bitfury_detect(void)
{
    int chip_count;
    int i;
    struct cgpu_info *bitfury_info;


    signal(SIGSEGV, sig_handler);
    signal(SIGILL, sig_handler);

    bitfury_info = calloc(1, sizeof(struct cgpu_info));
    bitfury_info->drv = &bitfury_drv;
    bitfury_info->threads = 1;

    applog(LOG_INFO, "INFO: bitfury_detect");
    chip_count = libbitfury_detectChips(bitfury_info->devices);
    if (!chip_count) {
        applog(LOG_WARNING, "No Bitfury chips detected!");
        return;
    } else {
        applog(LOG_WARNING, "BITFURY: %d chips detected!", chip_count);
    }

    bitfury_info->chip_count = chip_count;
    add_cgpu(bitfury_info);
}

static uint32_t bitfury_checkNonce(struct work *work, uint32_t nonce)
{
    applog(LOG_INFO, "INFO: bitfury_checkNonce");
}

static int bitfury_submitNonce(thr_info_t *thr, bitfury_device_t *device, struct timeval *now, struct work *owork, uint32_t nonce)
{
    int i;
    int is_dupe = 0;

    for (i=0; i < 32; i++) {
        if(device->nonces[i] == nonce) {
            is_dupe = 1;
            break;
        }
    }

    if(!is_dupe) {
        submit_nonce(thr, owork, nonce);
        device->nonces[device->current_nonce++] = nonce;
        if(device->current_nonce > 32)
            device->current_nonce = 0;
        i = device->stat_counter++;
        device->stat_ts [i] = now->tv_sec;
        device->stat_tsf[i] = tv2mcs(now);

        if (device->stat_counter == BITFURY_STAT_N)
            device->stat_counter = 0;
    }

    return (!is_dupe);
}


int bitfury_findChip(bitfury_device_t *devices, int chip_count, int slot, int fs) {
    int n;
    for (n = 0; n < chip_count; n++) {
        if ( (devices[n].slot == slot) && (devices[n].fasync == fs) )
            return n;
    }
    return -1;
}

void bitfury_setChipClk(bitfury_device_t *devices, int chip_count, int slot, int fs, int osc_bits) {
    int n = bitfury_findChip(devices, chip_count, slot, fs);
    if ( n >= 0 ) {
         // devices[n].osc6_bits = osc_bits;
         devices[n].osc6_bits_upd = osc_bits;
         devices[n].fixed_clk = true;
         applog(LOG_WARNING, "INFO: for chip %d assigned osc6_bits = %d", n, osc_bits);
    }
    else {
        applog(LOG_WARNING, "FATAL: chip %d not detected in slot %d", fs, slot);
    }
}

void bitfury_setSlotClk(bitfury_device_t *devices, int chip_count, int slot, int *fs_list) {

    int n;
    for ( n = 0; ( fs_list[n] >= 0 ) && ( n < BITFURY_BANKCHIPS ); n ++ ) {
        int fs = fs_list[n];
        int osc_bits = fs & 0xFF; // low 8 bits
        fs = fs >> 8; // high 24 bits is slot
        bitfury_setChipClk (devices, chip_count, slot, fs, osc_bits);
    }
}


double tv_diff(PTIMEVAL a, PTIMEVAL b) {
    double diff = tv2mcs(a) - tv2mcs(b);
    if (diff < 0) diff += 24.0 *3600.0 * 1e6; // add one day
    return diff;
}


inline void test_reclock(bitfury_device_p dev) {

    if ( dev->osc6_bits != dev->osc6_bits_upd ) {
        applog(LOG_WARNING, " for slot %X chip %X, osc6_bits changed from %d to %d, csw_count = %3d, cch_stat = { %2d %2d %2d %2d } ",
                               dev->slot, dev->fasync, dev->osc6_bits, dev->osc6_bits_upd, dev->csw_count,
                               dev->cch_stat[0], dev->cch_stat[1], dev->cch_stat[2], dev->cch_stat[3] );
         dev->osc6_bits = dev->osc6_bits_upd;
         send_freq( dev->slot, dev->fasync, dev->osc6_bits );
         cgtime (&dev->rst_time);
         dev->csw_count ++;
         dev->csw_back = 0;


    }
}

void init_devices (bitfury_device_t *devices, int chip_count) {
    int i;
    bitfury_device_p dev;


    for (i = 0; i < chip_count; i++) {
        dev = &devices[i];

#ifdef FAST_CLOCK1
            dev->osc6_bits = 53;
            if (!dev->osc6_bits_upd) dev->osc6_bits_upd = 53;
#else
            dev->osc6_bits = 54;
            if (!dev->osc6_bits_upd) dev->osc6_bits_upd = 54; // если не задано через опции командной строки
#endif

#ifndef BITFURY_AUTOCLOCK
            dev->fixed_clk = true;
#endif
            dev->rbc_stat[0] = dev->rbc_stat[1] = dev->rbc_stat[2] = dev->rbc_stat[3] = 0;
        }

    if (1) { // alpet: подстройка моих чипов (известные оптимумы)
        // overclocking/downclocking
        // 0x036, 0x136, 0x236, 0x336, 0x436, 0x536, 0x636


        int slot_0 [] = { -1 };
        int slot_1 [] = { -1 };
        int slot_2 [] = { -1 };
        int slot_3 [] = { -1 };
        int slot_4 [] = { -1 };
        int slot_5 [] = { -1 };
        int slot_6 [] = { -1 };
        int slot_7 [] = { -1 };
        int slot_8 [] = { -1 };
        int slot_9 [] = { -1 };
        int slot_A [] = { -1 };
        int slot_B [] = { -1 };
        int slot_C [] = { -1 };
        int slot_D [] = { -1 };
        int slot_E [] = { -1 };
        int slot_F [] = { -1 };

        int *all_slots[] = { slot_0, slot_1, slot_2, slot_3, slot_4, slot_5, slot_6, slot_7, slot_8, slot_9, slot_A, slot_B, slot_C, slot_D, slot_E, slot_F, NULL };

        for (i = 0; ( i < BITFURY_MAXBANKS ) && all_slots[i]; i ++)
             bitfury_setSlotClk(devices, chip_count, i, all_slots[i] );
    }

    for (i = 0; i < chip_count; i++) {

        dev = &devices[i];
        send_reinit(dev->slot, dev->fasync, dev->osc6_bits);
        cgtime (&dev->rst_time);
    }
}


void get_opt_filename(char *filename) {
    if ( getenv("HOME") && *getenv("HOME") ) {
            strcpy(filename, getenv("HOME"));
            strcat(filename, "/");
            mkdir(filename, 0777);
    }
    else
        strcpy(filename, "");
#ifdef BFGMINER_MOD
    strncat(filename, ".bfgminer/", PATH_MAX);
#else
    strncat(filename, ".cgminer/", PATH_MAX);
#endif

    mkdir(filename, 0777);
    strncat(filename, "bitfury_opt.conf", PATH_MAX);
}


void load_opt_conf (bitfury_device_t *devices, int chip_count) {
    char filename[PATH_MAX];
    get_opt_filename(filename);
    FILE *fcfg = fopen(filename, "r");
    if (!fcfg) return;

    applog(LOG_WARNING, "loading opt configuration from %s ", filename);

    int lcount = 0;

    int base_bits = BASE_OSC_BITS;


    while ( ! feof(fcfg) ) {
        char line [1024] = { 0 };
        fgets (line, 1024, fcfg);

        char *s = strstr(line, "base_bits=");
        if (s) {
            if (1 == sscanf(s, "base_bits=%d", &base_bits))
                applog(LOG_WARNING, "base_bits loaded from config = %d", base_bits);
            continue;
        }

        s = strstr(line, "slot_");
        if ( !s ) continue;
        lcount ++;

        s[4] = 32; // 'slot_XX=' -> 'slot XX='
        char *t = strtok(s, "=");
        int n_slot = 0, n_chip = -1;

        if (!t || strlen(t) < 1 ) {
            applog(LOG_WARNING, "cannot locate = in line %s", s);
            continue;
        }

        // applog(LOG_WARNING, "parsing line %d, 1-st token: \t%s", lcount, t);

        char tmp[100];

        if ( sscanf (s, "%s %X", tmp, &n_slot) < 2 ) {
            applog(LOG_WARNING, "parsing error at slot number detect");
            continue;
        }

        if (!base_bits)
            base_bits = BASE_OSC_BITS;

        slot_base_bits[n_slot] = base_bits;


        t = strtok(NULL, ";");
        while (t && strlen(t) > 10 ) {
            // applog(LOG_WARNING, "parsing line %d, next token: %35s", lcount, t);

            int v[4];
            int tc = sscanf(t, "%d:[%d,%d,%d,%d]@{%*.2f,%*.2f,%*.2f,%*.2f}", &n_chip, &v[0], &v[1], &v[2], &v[3]);
            if ( tc >= 5 ) {

                if ( n_chip < 0 ) break;
                int i = bitfury_findChip (devices, chip_count, n_slot, n_chip);
                if ( i >= 0 ) {
                    bitfury_device_p dev = &devices[i];
                    memcpy( dev->cch_stat, v, sizeof(v) ); // update stat
                    int best = 0;
                    // поправка лучшего битклока по количеству выборов
                    for (i = 0; i < 4; i ++)
                        if ( best < v[i] ) {
                             best = v[i];
                             dev->osc6_bits_upd = base_bits + i;
                        }
               }
            }
            else {
                applog(LOG_WARNING, "parsing error for token %s, sscanf returns %d", t, tc);
                break;
            }
            t = strtok(NULL, ";");
        }  // while 2
    } // while 1

    fclose(fcfg);
}


void save_opt_conf (bitfury_device_t *devices, int chip_count) {
    FILE *fcfg;
    char filename[PATH_MAX];
    if (!chip_count) return;

    get_opt_filename(filename);
    applog(LOG_WARNING, "dumping opt configuration to %s ", filename);

    fcfg = fopen(filename, "w");
    int i;
    int last_slot = -1;
    char line[1024] = { 0 };


    for (i = 0; i < chip_count; i ++) {
        bitfury_device_p dev = &devices[i];

        if (dev->slot != last_slot) {
            int bits = slot_base_bits[dev->slot];
            if (bits)
                fprintf(fcfg, "base_bits=%d\n", bits);

            if (strlen(line))
                fprintf(fcfg, "slot_%X=%s\n", last_slot, line);
            last_slot = dev->slot;
            line[0] = 0;
        }

        char dev_stat[128];
        sprintf(dev_stat, "%d:[%d,%d,%d,%d]@", dev->fasync, dev->cch_stat[0], dev->cch_stat[1], dev->cch_stat[2], dev->cch_stat[3]);
        strncat(line, dev_stat, 1024);

        float v0, v1, v2, v3;
        v0 = dev->rbc_stat[0];
        v1 = dev->rbc_stat[1];
        v2 = dev->rbc_stat[2];
        v3 = dev->rbc_stat[3];

        sprintf(dev_stat, "{%.2f,%.2f,%.2f,%.2f}; ", v0, v1, v2, v3);
        strncat(line, dev_stat, 1024);
    }

    fprintf(fcfg, "slot_%X=%s\n", last_slot, line);
    fclose(fcfg);
}


// #undef PREFETCH_WORKS

#ifdef PREFETCH_WORKS
int next_prefetch(int i) {
    return ( i + 1 ) % PREFETCH_WORKS;
}



inline int works_prefetched (struct cgpu_info *cgpu) {
    int i, cnt = 0;
    for (i = 0; i < PREFETCH_WORKS; i ++)
        if ( cgpu->prefetch[i] ) cnt ++;
    return cnt;
}



struct work* load_prefetch(struct cgpu_info *cgpu){
    int i;
    struct work* result = NULL;

    // rd_lock(&cgpu->qlock);
    // выборка задания из большой очереди
    // TODO: вместо блокировки здесь нужны атомарные операции!
    for (i = 0; i < PREFETCH_WORKS; i ++)  {
        if ( cgpu->prefetch [cgpu->r_prefetch]  ) {
            result = cgpu->prefetch [cgpu->r_prefetch];
            cgpu->prefetch [cgpu->r_prefetch] = NULL; // больше не выдавать
            break;
        }
        cgpu->r_prefetch = next_prefetch ( cgpu->r_prefetch );
    }
    // rd_unlock(&cgpu->qlock); // */
    return result;
}

static bool bitfury_fill(struct cgpu_info *cgpu) {
    bool ret;
    int i;

    struct work* nw = NULL;
#ifdef BITFURY_HARD_LOAD
    int max_need = MAX (4, cgpu->chip_count / 2);
#else
    int max_need = 10;
#endif

    if (max_need > PREFETCH_WORKS)
        max_need = PREFETCH_WORKS;

    int now_need =  ( max_need - works_prefetched(cgpu) );
    ret = ( now_need <= 0 ); // need find optimal values

    if (ret) return ret;
    nw = get_queued (cgpu);
    if (NULL == nw) return false;
    rd_lock(&cgpu->qlock); // don't return before unlock(!)
    for (i = 0; i < PREFETCH_WORKS; i ++)  {
        if ( NULL == cgpu->prefetch [cgpu->w_prefetch] ) {
            cgpu->prefetch [cgpu->w_prefetch] = nw;
            now_need --;
            break;
        }
        cgpu->w_prefetch = next_prefetch ( cgpu->w_prefetch );
    }

    ret = ( works_prefetched(cgpu) >= max_need ); // need find optimal values
    rd_unlock(&cgpu->qlock);



    return ret;
    // */
}


#endif

int work_push(cgpu_info_t *cgpu, bitfury_device_p dev) {
    dev->job_switched = 0;
    if ( dev->work == NULL )
    {
        // struct work *qwork  = cgpu->queued_work;

#ifdef  PREFETCH_WORKS
        dev->work = load_prefetch (cgpu);
        if (dev->work == NULL)           // no prefetched
#endif
            dev->work = get_queued(cgpu);

        if (NULL == dev->work) return 0;

        cgtime(&dev->work_start);
        work_to_payload(&(dev->payload), dev->work); // это задание будет теперь погружаться в буфер, при каждом обмене

        if (dev->work_end.tv_sec > 0) {
            double diff = tv_diff (&dev->work_start, &dev->work_end); // сколько прошло перед работой

            if ( ( diff > 0 ) && ( diff < 1e6 ) ) {
                if (dev->work_wait == 0)
                    dev->work_wait = diff;
                else
                    dev->work_wait = dev->work_wait * 0.993 + diff * 0.007; // EMA
            }
        }

        return 2;
    }
    return 1;
}


inline uint64_t work_receive(thr_info_t *thr, bitfury_device_p dev) {



    if (dev->job_switched && dev->work); else return 0;

    //
    struct timeval now;
    int nonces_cnt = 0;


    int j;
    int *res = dev->results;
    struct work *work = dev->work;
    struct work *owork = dev->owork;
    struct work *o2work = dev->o2work;
    cgtime(&now);

    // новое задание - считается - закончено?
    // work=>>owork=>>o2work
    for (j = dev->results_n-1; j >= 0; j--) {
        if (owork) {
            nonces_cnt += bitfury_submitNonce(thr, dev, &now, owork, bswap_32(res[j]) );
        }
    }
    dev->results_n = 0;
    dev->job_switched = 0;

    if ( (dev->old_num > 0) && o2work) {
        for(j = 0; j < dev->old_num; j++)
            nonces_cnt += bitfury_submitNonce(thr, dev, &now, o2work, bswap_32(dev->old_nonce[j]) );

        dev->old_num = 0;
    }

    if (dev->future_num > 0) {
        for(j=0; j < dev->future_num; j++)
            nonces_cnt += bitfury_submitNonce(thr, dev, &now, work, bswap_32(dev->future_nonce[j]) );
        dev->future_num = 0;
    }

    if (o2work) {
        work_completed(thr->cgpu, o2work);
        double diff = tv_diff (&now, &dev->work_start);
        dev->work_end = now;

        if (dev->work_median == 0)
            dev->work_median = diff;
        else
            dev->work_median = dev->work_median * 0.993 + diff *0.007; // EMA
    }



    // сдвиг миниочереди
    dev->o2work = dev->owork;
    dev->owork = dev->work;
    dev->work = NULL;
    dev->works_shifted ++;     // для перестановки задания в конец живой очереди
    dev->matching_work += nonces_cnt;
    dev->found_last = 0;
    test_reclock(dev); // думаю здесь самое лучшее место, чтобы чип перенастроить на другую частоту
    work_push(thr->cgpu, dev);   // это всего-лишь попытка, возможно неудачная!
    return 0xffffffffull * nonces_cnt;

}

uint64_t works_receive(thr_info_t *thr, bitfury_device_t *devices, int chip_count) {
    uint64_t hashes = 0;
    int chip;
    for (chip = 0;chip < chip_count; chip++)
        hashes += work_receive (thr, &devices[chip]);

    return hashes;
}

int works_push(cgpu_info_t *cgpu) {
    // подготовка заданий для чипов
    int chip;
    int w_pushed = 0;
    for (chip = 0; chip < cgpu->chip_count; chip++) {

       int code = work_push(cgpu, &cgpu->devices[last_chip]);

       if ( 2 == code ) w_pushed ++;

       if ( 0 == code ) {
           no_work ++;
           return 0;
       }
       last_chip ++;
       if (last_chip >= cgpu->chip_count)
           last_chip = 0;
    }

    return w_pushed + 1;

}

void dump_histogram(short *stat, char *buff, size_t buff_sz) {
    int i;
    short v;
    char line[512];

    for (i = 0; i < 50; i ++)
        if ( v = stat[i] ) {
            int n;
            size_t l = strlen(buff);
            if (l >= buff_sz) break;

            sprintf (line, "\t%.1f = (%3d):  ", 0.1 * (float)i, v);
            for (n = 0; n < v; n ++)
                 strncat(line, "*", 512);
            strncat (line, "\t\n", 512);
            strncat (buff, line, buff_sz);
        }

    // size_t l = strlen(buff);
    // sprintf(line, "buff_sz = %d, strlen = %d \n", buff_sz, l);
    // strcat (buff, line);
}

void dump_lines(char *buff) {
    char *line;
    line = strtok(buff, "\n");
    while (NULL != line) {
        applog(LOG_WARNING, "%s", line);
        line = strtok(NULL, "\n");
    }
}

void dump_chip_eff (bitfury_device_p dev, int ridx) {
    short *stat = dev->big_stat[ridx];
    float median = 0;
    float count = 0;
    size_t l;
    for (l = 0; l < 50; l ++) {
        if (stat[l] < 5) continue; // не существенные результаты
        count += (float) stat[l];
        median += 0.1 * (float) ( l * stat[l] );
    }

    if (count > 0) dev->eff_speed = median / count;

#ifdef BITFURY_CHIP_STAT
    char buff[16384];
    // get_datestamp ( buff, 100, get_cgtime() );
    format_time ( NULL, buff );
    char filename[PATH_MAX];
    strcpy(filename, "/var/log/bitfury/");
    mkdir(filename, 0777);
    l = strlen(filename);
    snprintf(filename + l, PATH_MAX - l, "slot%X_chip%X.log", dev->slot, dev->fasync);
    static char last_hour = 0;
    FILE *f;

    if (last_hour > g_time.tm_hour)
        f = fopen(filename, "w");     // at local midnight file rewrites
    else
        f = fopen(filename, "a");

    last_hour = g_time.tm_hour;

    if (!f) {
        applog(LOG_WARNING, "Cannot open file %s for append", filename);
        return;
    }

    fprintf(f, "%s --------------------- \n", buff);
    buff [0] = 0; // prepare for histogram
    dump_histogram ( stat, buff, 16384 );
    fprintf(f, "%s", buff);
    fprintf(f, "osc6_bits = %d, eff_speed = %.2f Gh/s, hw_rate = %.1f%% \n", BASE_OSC_BITS + ridx, dev->eff_speed, dev->hw_rate);
    fclose(f);
#endif

}


int ghash2int(double ghash) {
    int result = (int) round ( ghash * 10 );
    return MIN (result, 49);
}

int relative_bits_index(bitfury_device_p dev) {
    int ridx = ( dev->osc6_bits - BASE_OSC_BITS );
    ridx = MAX (ridx, 0);
    return MIN (ridx, 3);
}


void freq_bruteforce(bitfury_device_p dev) {
    int ridx = relative_bits_index(dev);
    int new_clk = ridx;
    float best = 3; // extremum Ghz for 54 clk
    best = dev->rbc_stat[ridx];
    int i, csum = 0;
    int test_count = 4;
    for (i = 0; i < 4; i ++) csum += dev->cch_stat[i];
    if ( csum > 2 )
         test_count = 2;

    if ( dev->csw_count < test_count ) {

        int optimal = 1;
        if ( csum > 4 ) optimal = dev->cch_stat[ridx]; // probably best choice

        new_clk = ( ridx + 1 ) & 3; // masked enum
        while ( csum > 2 && dev->cch_stat[new_clk] < optimal )
                new_clk = ( new_clk + 1 ) & 3; // дополнительные циклы - пропуск неоптимальных выборов
    }
    else
    if ( best < 4 && dev->csw_count < test_count + 1 ) {
        // однократный(!) поиск наилучшего, для работы с заданным клоком.
        // if ( stat_dumps > 150 ) best = 3;
        int i;
        for (i = 0; i < 4; i ++) {
            if ( best >= dev->rbc_stat [i] ) continue;
            best = dev->rbc_stat [i];
            new_clk = i; // optimus
        } // for

        // подведение итогов соревнования
        // если произошла смена к удачной конфигурации или было мало регистраций выбора
        if ( ridx != new_clk || dev->cch_stat[new_clk] < 2 ) dev->cch_stat[new_clk] ++;
    }

    new_clk = new_clk + BASE_OSC_BITS;

    if ( dev->osc6_bits_upd != new_clk ) {
         dev->osc6_bits_upd = new_clk;
         test_reclock(dev);
    }
}


inline double speed_in_period(double *stat, double range_mcs, double now_mcs, int *shares_found) {
    *shares_found = calc_stat_f(stat, range_mcs, now_mcs);
    return shares_to_ghashes(*shares_found, range_mcs / 1e6 );
}



double collect_chip_stats (bitfury_device_p dev, int loop) {

    double limit_frame = (double)short_stat * 16 * 1e6;  //
    // статистику стоит оценивать от последнего сброса устройства, иначе хрень будет.
    double now_mcs = tv2mcs ( get_cgtime() );
    double rst_msc = tv2mcs ( &dev->rst_time );
    double elps_eff = now_mcs - rst_msc;

    if (elps_eff > limit_frame)
        elps_eff = limit_frame; // 320 seconds limit

    int shares_found = 0;

    double ghash = speed_in_period(dev->stat_tsf, elps_eff, now_mcs, &shares_found);

    int i_chip = dev->fasync;
    int n_slot = dev->slot;
    int len;

    double alt_gh;
    char *s_line = stat_lines[n_slot];


    // if slot changed
    if ( dev->fasync == 0 ) {
        sprintf(s_line, "[%X] ", n_slot);
    #ifdef BITFURY_MONITORING
        float slot_temp = 60; // tm_i2c_gettemp(n_slot) * 0.1;
        float slot_vc0 = tm_i2c_getcore0(n_slot) * 1000;
        float slot_vc1 = tm_i2c_getcore1(n_slot) * 1000;

        if (stat_dumps > 2) {
            // checking anomaly extremums 0.2 outbound
            if (slot_vc0 < 850) slot_vc0 = 850;
            if (slot_vc1 < 850) slot_vc1 = 850;
            if (slot_vc0 > 2000) slot_vc0 = 1090;
            if (slot_vc1 > 2000) slot_vc1 = 1090;

            slot_vc0 = vc0_median[n_slot] * 0.95 + slot_vc0 * 0.05;
            slot_vc1 = vc1_median[n_slot] * 0.95 + slot_vc1 * 0.05;
        }

        vc0_median[n_slot] = slot_vc0;
        vc1_median[n_slot] = slot_vc1;

        // sprintf(stat_lines[n_slot], "[%X] T:%3.0f | V: %4.0f %4.0f| ", n_slot, slot_temp, slot_vc0, slot_vc1);
        // sprintf(s_line, "[%X] T:%3.0f | V: %4.2f %4.2f| ", n_slot, slot_temp, slot_vc0 / 1000, slot_vc1 / 1000);
        // вывод температуры бесполезен для плат Метабанка, значение практически не связанно с уровнем нагрева чипов.
        sprintf(s_line, "[%X] V: %4.2f %4.2f| ", n_slot, slot_vc0 / 1000, slot_vc1 / 1000);
    #endif

    }

    len = strlen(stat_lines[n_slot]);

    dev->csw_back ++;

    // if ( stat_dumps <= 2 || dev->csw_back <= 2 ) ghash *= 0.5; // из-за заполнения очередей, тут перебор тот ещё
    alt_gh = ghash;

    float hw_errs = (float) dev->hw_errors;
    float saldo = hw_errs + shares_found; // TODO: проверить, нужно ли добавить режики?


    if ( dev->work_median > 0 )
         alt_gh = 3e6 / dev->work_median;

    if (saldo > 0)
       hw_errs = 100 * hw_errs / saldo;
    else
       hw_errs = 0;

    if (stat_dumps < 5)
       dev->hw_rate = hw_errs;
    else
       dev->hw_rate = dev->hw_rate * 0.93 + hw_errs * 0.07; // EMA 16

    int ridx = dev->osc6_bits - BASE_OSC_BITS;
    dev->rbc_stat[ridx] = ghash;

    char *cl_tag = " ";


    if ( ghash <= 2.0 ) cl_tag = " -";
    if ( ghash <= 1.5 ) cl_tag = "--";
    if ( ghash <= 1.0 ) cl_tag = " !";
    if ( ghash <= 0.5 ) cl_tag = "!!";

    if ( ghash >= 3.0 ) cl_tag = " +";
    if ( ghash >= 4.0 ) cl_tag = "++";

    int rr = ghash2int (ghash);
    chips_by_rate [rr] ++;      // не более chip_count сумма по всем массиву быть должна
    if (dev->csw_back > 12) // если после переключения прошло много времени, и производительность стабилизировалась.
        dev->big_stat[ridx][rr] ++; // для получения детального отчета по чипу


    if ( loop < 15 ) {

        if ( loop > 13  && dev->work_median > 0 )
            snprintf(s_line + len, STAT_LINE_LENGTH - len, "%2.0f @ @%2d | ", 10 * alt_gh, dev->osc6_bits ); // speed from work-time, wait time
        else
            snprintf(s_line + len, STAT_LINE_LENGTH - len, "%2s%2d -%5.1f | ", cl_tag, rr, dev->hw_rate ); // speed and errors
    }
    else {

        // сброс статистики в файл, рассчет среднего хэшрейта за все время(!)
        dump_chip_eff (dev, ridx);
        if ( dev->eff_speed > 0 )
             dev->rbc_stat[ridx] = dev->eff_speed;


        char s[] = "     ";
        s[ridx] = 0x5B;
        s[ridx + 1] = 0x5D;

        float h0 = dev->rbc_stat[0] * 10;
        float h1 = dev->rbc_stat[1] * 10;
        float h2 = dev->rbc_stat[2] * 10;
        float h3 = dev->rbc_stat[3] * 10;
        snprintf( stat_lines[n_slot] + len, STAT_LINE_LENGTH - len, "%c%2.0f%c%2.0f%c%2.0f%c%2.0f| ", s[0], h0, s[1], h1, s[2], h2, s[3], h3, s[4] ); // intermediate dump clock
    }

    shares_total += shares_found;
    shares_first += i_chip  < BITFURY_BANKCHIPS/2 ? shares_found : 0;
    shares_last  += i_chip >= BITFURY_BANKCHIPS/2 ? shares_found : 0;

    dev->hw_errors = 0;


    return ghash;

}

void check_not_hang(bitfury_device_p dev, double speed) {

    static unsigned recovers = 0;
#ifdef  BITFURY_AUTOCLOCK
    if ( dev->csw_back > 50 && dev->eff_speed > 0 && dev->eff_speed < LOW_HASHRATE) dev->fixed_clk = false;
    if ( dev->csw_back > 100 && speed > 1.0 && speed < LOW_HASHRATE && !dev->fixed_clk ) {
        dev->fixed_clk = false;
        dev->csw_count = 0;
        printf(CL_LT_RED);
        applog(LOG_WARNING, "#WARNING: Chip at %x x %x has low median hashrate, auto-clock reset ", dev->fasync, dev->slot );
        printf(CL_RESET);
        int i;
        for (i = 0; i < 3; i ++) dev->rbc_stat[i] = 0; // затереть статистику, типа устарела
    }
#endif

    if ( speed <= 1.2 || ( speed < 1.7 && speed < dev->prv_speed  ) ) {
        if ( dev->csw_back > 4 ) dev->alerts ++;
    }
    else dev->alerts = 0;

    dev->prv_speed = speed;

    // сброс чипа по совсем уж малому хэшрейту
    if ( 3 < dev->alerts ) {
        recovers ++;
        printf(CL_LT_RED);
        applog(LOG_WARNING, "Slot %X chip %X FREQ CHANGE-RESTORE, total recovers = %d", dev->slot, dev->fasync, recovers);
        printf(CL_RESET);
        send_shutdown(dev->slot, dev->fasync);
        nmsleep(100);
        send_reinit(dev->slot, dev->fasync, 53); // fail-safe
        dev->fixed_clk = false;
        dev->alerts = 0;
        dev->csw_back = 0;
        dev->csw_count ++;

        memset (dev->oldbuf, 0, sizeof(dev->oldbuf));
        memset (dev->tsvals, 0, sizeof(dev->tsvals));

        cgtime (&dev->rst_time);
        dev->cch_stat[0] = dev->cch_stat[1] = dev->cch_stat[2] = dev->cch_stat[3] = 0; // полный сброс статистики автоподбора
    }

}

/* ========================================================================================================================================================= */

static int64_t try_scanHash(thr_info_t *thr)
{

    static bitfury_device_t *devices, *dev; // TODO Move somewhere to appropriate place
    int chip_count;
    int chip, i;
    uint64_t hashes = 0;
    static struct timeval now;
    static struct timeval last_call;
    static double call_period = 0;

#define BIG_LINE_LENGTH 16384
    unsigned char line[BIG_LINE_LENGTH];
    static time_t short_out_t = 0;
    static double short_out_tf = 0;
    int long_stat = 900;
    static time_t long_out_t = 0;
    int long_long_stat = 60 * 30;
    static time_t long_long_out_t;
    static double median_load = 0;
    double elps_mcs = 0;
    double now_mcs = 0;
    double load_mcs = 0;
    static int interval = 0;
    static unsigned busy_count = 0;
    static unsigned ready_count = 0;

    loops_count ++;
    call_count ++;

    static char debug_log[1024] = { 0 };
    static int active_slot = 0;
    static char *chips_in_slot[BITFURY_MAXBANKS] = { 0 };

    devices = thr->cgpu->devices;
    chip_count = thr->cgpu->chip_count;

#ifdef  USE_LIVE_ORDER
    static bitfury_device_p live_devs[BITFURY_MAXCHIPS] = { NULL };
    // в первый цикл заполнить по порядку
    for (i = 0; 1 == loops_count && i < chip_count; i ++  )
         live_devs[i] = &devices[i];
#endif

    cgtime(&now);


    if ( loops_count == 1 ) {
         init_devices  (devices, chip_count);
         load_opt_conf (devices, chip_count);

         for (i = 0; i < chip_count; i ++)
              chips_in_slot [ devices[i].slot ] ++;
    }


    if ( loops_count > 2 ) {
        elps_mcs = tv_diff (&now, &last_call); //
        if ( call_period == 0 )
             call_period = elps_mcs;
        else
             call_period = call_period * 0.999 + elps_mcs * 0.001;

    }

    last_call = now;
    cgtime(&last_call);

    // hashes += works_receive(thr, devices, chip_count);
    works_push (thr->cgpu);

#ifdef BITFURY_HARD_LOAD
    nmsleep(150);
    libbitfury_sendHashData(thr, devices, chip_count);
    hashes += works_receive(thr, devices, chip_count);
#else
    int last_slot = -1;
 #ifdef USE_LIVE_ORDER
    int tmp;

    for (tmp = 0; tmp < 10; tmp ++) {
        libbitfury_sendHashOne (thr, live_devs[0], &last_slot); // 3-5ms load
        uint64_t recv = work_receive (thr, live_devs[0]);
        hashes += recv;
        bitfury_device_p ld = live_devs[0];

        nusleep (interval);
        // !стратегия - поощряет лучшие чипы, ставя их ближе к началу очереди (ну или в середину).
        //  стратегия - поощряет плохие чипы...

        if ( !ld->works_shifted ) {
            // сдвиг всей очереди, запихивание "добитого" в конец
            int last = chip_count - 1;
            for ( i = 0; i < last; i ++)
                live_devs[i] = live_devs[i + 1];
            live_devs[last] = ld;
        }
        else {
            int near = chip_count / 2;

            // сдвиг части очереди, запихивание "недобитого" поближе
            for ( i = 0; i < near; i ++)
                // last iter.:  [half - 1] = [half]
                live_devs[i] = live_devs[i + 1];

            live_devs[near] = ld;

        }

        if (ld->works_shifted) {

            ld->works_shifted = 0;
            // if (recv) libbitfury_sendHashOne (thr, live_devs[0]); // 3-5ms load

            ready_count ++;
        }
        else
            busy_count ++;

    } // for tmp

 #else
    // nmsleep(10);
    for (i = 0; i < chip_count; i ++)
        if ( devices[i].slot == active_slot ) {
             bitfury_device_p d = &devices[i];
             libbitfury_sendHashOne (thr, d, &last_slot);
             hashes += work_receive (thr, d);
        }
    // выбор другого активного слота (!)
    do {
        active_slot ++;
        if (active_slot >= BITFURY_MAXBANKS)
            active_slot = 0;
    } while ( !chips_in_slot[active_slot] );
 #endif

 if (last_slot >= 0)
     tm_i2c_clear_oe(last_slot);
#endif


    cgtime(&now);


    now_mcs = tv2mcs (&now);

    if (!short_out_t) {
        short_out_t  = now.tv_sec;
        short_out_tf = now_mcs;
    }

    if (!long_out_t) long_out_t = now.tv_sec;

    if ( loops_count < 50 )
         return hashes; // обычно статистика не накапливается

    // рассчет времени основной нагрузки в микросекундах
    load_mcs = now_mcs - tv2mcs(&last_call);

    // struct timespec period = t_diff(begin, end);
    // load_mcs = ( period.tv_sec * 1000000000LU + period.tv_nsec ) * 0.001; // nsec to usec

    if (0 == median_load)
        median_load = load_mcs;
    else
        median_load = median_load * 0.98 + load_mcs * 0.02; //

    int elapsed = now.tv_sec - short_out_t;


    if (elapsed >= short_stat) {
        elps_mcs = now_mcs - short_out_tf;
        short_out_tf = now_mcs;
        shares_first = shares_last = shares_total = 0;

        int len, k;
        double gh[BITFURY_MAXBANKS][BITFURY_BANKCHIPS] = {0};

        double ghsum = 0, gh1h = 0, gh2h = 0;
        stat_dumps ++;

        int maskv = stat_dumps & 15;
        if ( maskv == 15 ) printf("%s\n", CL_LT_WHITE);

        memset (chips_by_rate, 0, sizeof(short) * 50); // сбросить предыдущий счет
        memset (stat_lines, 0, sizeof(stat_lines) );

        for (chip = 0; chip < chip_count; chip++) {
           // сбор статистики по чипам

           bitfury_device_p dev = &devices[chip];
           double speed = collect_chip_stats  (dev, maskv);         // for (chip; chip < n-chip; chip++)

#ifdef  BITFURY_AUTOCLOCK
           // AUTOFREQ: переключение частоты осциллятора принудительно (в режиме брутфорс или выбора лучшего)
           if ( ( dev->alerts >= 3 || ( dev->csw_back > 80 && maskv == 15 ) ) && !dev->fixed_clk )
                freq_bruteforce (dev);
#endif
           gh[dev->slot][dev->fasync] = speed;


           // для проверки на зависания лучше оценивать последний период 60с
           speed = speed_in_period(dev->stat_tsf, 60 * 1e6, now_mcs, &k);
           check_not_hang (dev, speed); // проверки на слишком маленькую частоту
        }


        if (maskv == 15) {
            save_opt_conf(devices, chip_count);
            // interval += 10;
            if (interval > 1500) interval = 0;
        }

#ifdef BITFURY_ENABLE_SHORT_STAT
        // printing histogram
        sprintf (line, "Just %d chips by rate stats:\t\t\t\t", chip_count);
        applog(LOG_WARNING, "%s", line);
        line [0] = 0;
        dump_histogram(chips_by_rate, line, BIG_LINE_LENGTH);
        dump_lines(line);
        int pcount = 0;

#ifdef PREFETCH_WORKS
        pcount = works_prefetched(thr->cgpu);
#endif
        // sprintf(line, "vvvvwww SHORT stat %ds: wwwvvvv", short_stat);
        sprintf(line, "  ================== SHORT stat, elapsed %.3fs, no_work = %d, prefetched = %d, dump %d, call period = %.2f ms, count = %5d =================== ",
                                                 elps_mcs / 1e6, no_work, pcount, stat_dumps, call_period / 1000, call_count );
        no_work = 0;
        call_count = 0;


        applog(LOG_WARNING, line);
        double ghsm_saldo = 0;


        for(i = 0; i < BITFURY_MAXBANKS; i++)
            if(strlen(stat_lines[i])) {
                len = strlen(stat_lines[i]);
                ghsum = 0;
                gh1h = 0;
                gh2h = 0;


                for(k = 0; k < BITFURY_BANKCHIPS/2; k++) {
                    gh1h += gh[i][k];                       // saldo for 0..3 chip
                    gh2h += gh[i][k + BITFURY_BANKCHIPS/2]; // saldo for 4..7 chip

                }
                // snprintf(stat_lines[i] + len, 256 - len, "- %2.1f + %2.1f = %2.1f slot %i ", gh1h, gh2h, ghsum, i);
                ghsum = gh1h + gh2h;
                double ghmed = ghsum;
                snprintf(stat_lines[i] + len, 256 - len, " S: %4.1f + %4.1f = %4.1f  (%4.1f) [%X]", gh1h, gh2h, ghsum, ghmed, i);

                ghsm_saldo += ghmed;


                if (maskv < 18) {
                    if (i & 1 == 1)
                        printf(CL_LT_GREEN);
                    else
                        printf("\e[0m\r");
                }

                applog(LOG_WARNING, stat_lines[i]);
            }

        elapsed = now.tv_sec - long_out_t;
        printf("\e[37;40m\r");


        applog(LOG_WARNING, "Median hash-rate saldo = %4.1f, interval = %d mcs, seconds to long stat %5d, median_load = %.1f ms, busy = %7d, ready = %7d ",
                                ghsm_saldo, interval, long_stat - elapsed, median_load * 0.001, busy_count, ready_count );

        applog(LOG_WARNING, line);
        busy_count = ready_count = 0;
        // malloc_stats();
#endif
        short_out_t = now.tv_sec;

        if ( maskv == 15 ) printf("%s", CL_RESET);
    }
#ifdef BITFURY_ENABLE_LONG_STAT
    if (elapsed >= long_stat ) {
        long_out_t = now.tv_sec;

        int shares_first = 0, shares_last = 0, shares_total = 0;
        char stat_lines[BITFURY_MAXBANKS][256] = {0};
        int len, k;
        double gh[BITFURY_MAXBANKS][BITFURY_BANKCHIPS] = {0};
        double ghsum = 0, gh1h = 0, gh2h = 0;

        for (chip = 0; chip < chip_count; chip++) {
            dev = &devices[chip];
            int shares_found = calc_stat(dev->stat_ts, elapsed, now);
            double ghash;
            len = strlen(stat_lines[dev->slot]);
            ghash = shares_to_ghashes(shares_found, (double)long_stat);
            gh[dev->slot][chip % BITFURY_BANKCHIPS] = ghash;
            snprintf(stat_lines[dev->slot] + len, 256 - len, "%.2f-%3.0f ", ghash, dev->mhz);
            shares_total += shares_found;
            shares_first += chip < BITFURY_BANKCHIPS/2 ? shares_found : 0;
            shares_last += chip >= BITFURY_BANKCHIPS/2 ? shares_found : 0;
        }

        sprintf(line, "  !!!_________ LONG stat, elapsed %ds: ___________!!!", elapsed);
        // attron(A_BOLD);
        printf("%s", CL_LT_YELLOW);
        applog(LOG_WARNING, line);
        for(i = 0; i < BITFURY_MAXBANKS; i++)
            if(strlen(stat_lines[i])) {
                len = strlen(stat_lines[i]);
                ghsum = 0;
                gh1h = 0;
                gh2h = 0;
                for(k = 0; k < BITFURY_BANKCHIPS/2; k++) {
                    gh1h += gh[i][k];
                    gh2h += gh[i][k + BITFURY_BANKCHIPS/2];
                    ghsum += gh[i][k] + gh[i][k + BITFURY_BANKCHIPS/2];
                }
                snprintf(stat_lines[i] + len, 256 - len, "- %4.1f + %4.1f = %4.1f Gh/s slot %X ", gh1h, gh2h, ghsum, i);
                applog(LOG_WARNING, stat_lines[i]);
            }

        printf("%s", CL_RESET);

        // attroff(A_BOLD);
    }
#endif


    return hashes;
}

static int64_t bitfury_scanHash(thr_info_t *thr) {
     int64_t result = 0;

     /*
     int64_t hashes;
     int cc = thr->cgpu->chip_count;
     do {

          result += hashes;
     } while (hashes && result < 50); // */

     // загружать X мс полезной работой - теоретически меньше будет режиков.



#ifdef BITFURY_HARD_LOAD
     result = try_scanHash(thr);
#else
     double start_mcs = tv2mcs( get_cgtime() ) ;
     double elapsed = 0;
     do {
         result += try_scanHash(thr);
         elapsed = tv2mcs( get_cgtime() ) - start_mcs;
     } while ( elapsed < WORK_FRAME * 1000 );
#endif
     int ms = 1;
#ifdef BFGMINER_MOD
     nmsleep (ms);
#else
     nmsleep (1);
     // if ( 0 == result ) nmsleep ( BITFURY_SCANHASH_DELAY - (int)time_ms ); // strict loop time
#endif
     return result;
}


double shares_to_ghashes(int shares, double seconds) {
    return ( (double)shares * 4.294967296 ) / ( seconds );

}

int calc_stat(time_t * stat_ts, time_t stat, struct timeval now) {
    int j;
    int shares_found = 0;
    for(j = 0; j < BITFURY_STAT_N; j++) {
        if (now.tv_sec - stat_ts[j] < stat) {
            shares_found++;
        }
    }
    return shares_found;
}
int calc_stat_f (double * stat_tsf, double elapsed, double now_mcs) {
    int j;
    int shares_found = 0;
    for(j = 0; j < BITFURY_STAT_N; j++) {
        if (now_mcs - stat_tsf[j] < elapsed) {
            shares_found++;
        }
    }
    return shares_found;
}


static void bitfury_statline_before(char *buf, struct cgpu_info *cgpu)
{
    applog(LOG_INFO, "INFO bitfury_statline_before");
}


static void bitfury_shutdown(thr_info_t *thr)
{
    int chip_count;
    int i;

    chip_count = thr->cgpu->chip_count;

    applog(LOG_INFO, "INFO bitfury_shutdown");
    libbitfury_shutdownChips(thr->cgpu->devices, chip_count);
}

static void bitfury_disable(thr_info_t *thr)
{
    applog(LOG_INFO, "INFO bitfury_disable");
}


static void get_options(struct cgpu_info *cgpu)
{
    char buf[BUFSIZ+1];
    char *ptr, *comma, *colon, *colon2;
    size_t max = 0;
    int i, slot, fs, bits, chip, def_bits;

    int default_bits = BASE_OSC_BITS + 1;

#ifdef FAST_CLOCK1
    default_bits = 53;
#endif


    for(i=0; i<cgpu->chip_count; i++)
        cgpu->devices[i].osc6_bits_upd = default_bits; // this is default value

    if (opt_bitfury_clockbits == NULL) {
        buf[0] = '\0';
        return;
    }

    ptr = opt_bitfury_clockbits;

    do {
        comma = strchr(ptr, ',');
        if (comma == NULL)
            max = strlen(ptr);
        else
            max = comma - ptr;
        if (max > BUFSIZ)
            max = BUFSIZ;
        strncpy(buf, ptr, max);
        buf[max] = '\0';

        if (*buf) {
            colon = strchr(buf, ':');
            if (colon) {
                *(colon++) = '\0';
                colon2 = strchr(colon, ':');
                if (colon2)
                    *(colon2++) = '\0';
                if (*buf && *colon && *colon2) {
                    slot = atoi(buf);
                    fs = atoi(colon);
                    bits = atoi(colon2);
                    chip = bitfury_findChip(cgpu->devices, cgpu->chip_count, slot, fs);
                    if(chip > 0 && chip < cgpu->chip_count && bits >= 48 && bits <= 56) {
                        cgpu->devices[chip].osc6_bits_upd = bits;
                        applog(LOG_INFO, "Set clockbits: slot=%d chip=%d bits=%d", slot, fs, bits);
                    }
                }
            } else {
                def_bits = atoi(buf);
                if(def_bits >= 48 && def_bits <= 56) {
                    for(i=0; i<cgpu->chip_count; i++)
                        cgpu->devices[i].osc6_bits_upd = def_bits;
                }
            }
        }
        if(comma != NULL)
            ptr = ++comma;
    } while (comma != NULL);
} // */

static bool bitfury_prepare(thr_info_t *thr)
{
    struct timeval now;
    struct cgpu_info *cgpu = thr->cgpu;

    cgtime(&now);
#ifdef BFGMINER_MOD
    get_datestamp (cgpu->init, 40, (time_t) now.tv_sec);
#else
    get_datestamp (cgpu->init, &now);
#endif
    get_options(cgpu);

    applog(LOG_INFO, "INFO bitfury_prepare");
    return true;
}


static struct api_data *bitfury_api_stats(struct cgpu_info *cgpu)
{
    struct api_data *root = NULL;
    static bitfury_device_t *devices;
    struct timeval now;
    struct bitfury_info *info = cgpu->device_data;
    int shares_found, i;
    double ghash, ghash_sum = 0.0;
    unsigned int osc_bits;
    char mcw[24];
    uint64_t total_hw = 0;

    devices = cgpu->devices;
    root = api_add_int(root, "chip_count", &(cgpu->chip_count),false);
    cgtime(&now);

    for (i = 0; i < cgpu->chip_count; i++) {
        sprintf(mcw, "clock_bits_%d_%d", devices[i].slot, devices[i].fasync);
        osc_bits = (unsigned int)devices[i].osc6_bits;
        root = api_add_int(root, mcw, &(devices[i].osc6_bits), false);
    }
    for (i = 0; i < cgpu->chip_count; i++) {
        sprintf(mcw, "match_work_count_%d_%d", devices[i].slot, devices[i].fasync);
        root = api_add_uint(root, mcw, &(devices[i].matching_work), false);
    }
    for (i = 0; i < cgpu->chip_count; i++) {
        sprintf(mcw, "hw_errors_%d_%d", devices[i].slot, devices[i].fasync);
        root = api_add_uint(root, mcw, &(devices[i].hw_errors), false);
        total_hw += devices[i].hw_errors;
    }
    for (i = 0; i < cgpu->chip_count; i++) {
        // shares_found = calc_stat_f (devices[i].stat_tsf, BITFURY_API_STATS, tv2mcs(&now));
        shares_found = calc_stat(devices[i].stat_ts, BITFURY_API_STATS, now);
        ghash = shares_to_ghashes(shares_found, (double)BITFURY_API_STATS);
        ghash_sum += ghash;
        sprintf(mcw, "ghash_%d_%d", devices[i].slot, devices[i].fasync);
        root = api_add_double(root, mcw, &(ghash), true);
    }
    api_add_uint64(root, "total_hw", &(total_hw), false);
    api_add_double(root, "total_gh", &(ghash_sum), true);
    ghash_sum /= cgpu->chip_count;
    api_add_double(root, "avg_gh_per_chip", &(ghash_sum), true);

    return root;
}

static bool bitfury_init(struct thr_info *thr) {
    return true;
}

struct device_drv bitfury_drv = {
    .drv_detect = bitfury_detect,
    .thread_prepare = bitfury_prepare,
    .thread_init = bitfury_init,
    .scanwork = bitfury_scanHash,
    .thread_shutdown = bitfury_shutdown,
#ifdef BFGMINER_MOD
    .dname = "bitfury_gpio",
    .name = "BFY",
    .minerloop = hash_queued_work,
#else
    .drv_id = DRIVER_BITFURY,
    .dname = "bitfury",
    .name = "BITFURY",
    .hash_work = hash_queued_work,
    .get_statline_before = bitfury_statline_before,
#endif
#ifdef PREFETCH_WORKS
    .queue_full = bitfury_fill,
#endif
    .get_api_stats = bitfury_api_stats,
};
