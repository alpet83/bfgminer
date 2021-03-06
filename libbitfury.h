/**
 * libbitfury.h - library for Bitfury chip/board
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
**/

#ifndef __LIBBITFURY_H__
#define __LIBBITFURY_H__

#include "bitfury-config.h"

#ifdef BITFURY_METABANK


#define BITFURY_MAXCHIPS 200
#define BITFURY_MAXBANKS 16
#define BITFURY_BANKCHIPS 8
#define PREFETCH_WORKS 256

#endif


#ifdef BITFURY_NEEDBMW_NOMUX

#define BITFURY_MAXCHIPS 100
#define BITFURY_MAXBANKS 1
#define BITFURY_BANKCHIPS 100
#define BITFURY_SCANHASH_DELAY 100

#endif

#include "miner.h"


struct bitfury_payload {
	unsigned char midstate[32];
	unsigned int junk[8];
	unsigned m7;
	unsigned ntime;
	unsigned nbits;
	unsigned nnonce;
};

struct bitfury_device {
	unsigned osc6_bits;
    unsigned osc6_req;
    unsigned osc6_bits_upd;
	unsigned newbuf[17];
	unsigned oldbuf[17];
    unsigned eqcntr[16]; // сколько раз при опросе были равны буфера
    unsigned acvals[16]; // принятые решения
    unsigned tsvals[16]; // уже проверенные значения
    int      index;      // индекс проверяемого значения


    struct work *work;
    struct work *owork;
    struct work *o2work;
    struct timeval work_start;
    struct timeval work_end;
    double work_median;  // median time in microseconds
    double work_wait;    // средний простой в ожидании работы
	int job_switched;
	struct bitfury_payload payload;
	struct bitfury_payload opayload;
	struct bitfury_payload o2payload;
	unsigned int results[16];
	int results_n;
    time_t stat_ts [BITFURY_STAT_N];
    double stat_tsf[BITFURY_STAT_N];
	unsigned int stat_counter;
    unsigned int future_nonce[16];
    unsigned int old_nonce[16];

    int    future_num;
    int    old_num;
    struct timespec ts1;
	struct timespec timer1;
	struct timespec timer2;
	struct timespec otimer1;
	struct timespec otimer2;
	struct timespec predict1;
	struct timespec predict2;
	unsigned int counter1, counter2;
	unsigned int ocounter1, ocounter2;
	int rate; //per msec
	int osc_slow;
	int osc_fast;
	int req1_done, req2_done;
	double mhz;
	double ns;
	unsigned slot;
	unsigned fasync;
	unsigned hw_errors;

    int              alerts;
    float            eff_speed;       // рассчитывается из big_stat
    float            prv_speed;       // для оценки "начала забастовки"
    float            hw_rate;
    bool             fixed_clk;    
    float            rbc_stat[4];     // rate-by-clock: 53, 54, 55, 56 associated to chip hash-rate
    short            big_stat[4][50];
    int              cch_stat[4];     // clock choice stat: сколько выбирался каждый клок по итогам соревнования
    int              csw_back;        // clock switch back: сколько статистики прошло, после переключения частоты
    int              csw_count;       // clock switch count: сколько раз сменялась частота принудительно
    struct timeval   rst_time;        // reset time: когда устройство сбрасывалось последний раз

	unsigned int matching_work;    
    unsigned int works_shifted;       // сколько заданий сдвинуто за последний цикл
    unsigned int found_last;
	unsigned int nonces[32];
	int current_nonce;
	double gh_stat[8];
};

typedef struct bitfury_device bitfury_device_t;
typedef bitfury_device_t *bitfury_device_p;
typedef struct timeval *PTIMEVAL;


void send_freq(int slot, int chip_index, int bits);
void send_reinit(int slot, int chip_index, int n);
void send_shutdown(int slot, int chip_index);

int libbitfury_readHashData(unsigned int *res);
void libbitfury_sendHashData(struct thr_info *thr, struct bitfury_device *bf, int chip_count);
void libbitfury_sendHashOne(struct thr_info *thr, struct bitfury_device *d, int *last_slot);
void work_to_payload(struct bitfury_payload *p, struct work *w);
struct timespec t_diff(struct timespec start, struct timespec end);
int libbitfury_detectChips(struct bitfury_device *devices);
int libbitfury_shutdownChips(struct bitfury_device *devices, int chip_count);

#endif /* __LIBBITFURY_H__ */
