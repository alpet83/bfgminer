/**
 * libbitfury.c - library for Bitfury chip/board
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

#include "config.h"

#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "miner.h"
#include "tm_i2c.h"
#include "libbitfury.h"

#include "spidevc.h"
#include "sha2.h"

#include <time.h>
#include "driver-config.h"

#define BITFURY_REFRESH_DELAY 100
#define BITFURY_DETECT_TRIES 3000 / BITFURY_REFRESH_DELAY


// 0 .... 31 bit
// 1000 0011 0101 0110 1001 1010 1100 0111

// 1100 0001 0110 1010 0101 1001 1110 0011
// C16A59E3

unsigned char enaconf[4] = { 0xc1, 0x6a, 0x59, 0xe3 };
unsigned char disconf[4] = { 0, 0, 0, 0 };

unsigned decnonce(unsigned in);

/* Configuration registers - control oscillators and such stuff. PROGRAMMED when magic number is matches, UNPROGRAMMED (default) otherwise */
void config_reg(int cfgreg, int ena)
{
	if (ena) spi_emit_data(0x7000+cfgreg*32, (void*)enaconf, 4);
	else     spi_emit_data(0x7000+cfgreg*32, (void*)disconf, 4);
}

#define FIRST_BASE 61
#define SECOND_BASE 4
char counters[16] = { 64, 64,
	SECOND_BASE, SECOND_BASE+4, SECOND_BASE+2, SECOND_BASE+2+16, SECOND_BASE, SECOND_BASE+1,
	(FIRST_BASE)%65,  (FIRST_BASE+1)%65,  (FIRST_BASE+3)%65, (FIRST_BASE+3+16)%65, (FIRST_BASE+4)%65, (FIRST_BASE+4+4)%65, (FIRST_BASE+3+3)%65, (FIRST_BASE+3+1+3)%65};

//char counters[16] = { 64, 64,
//	SECOND_BASE, SECOND_BASE+4, SECOND_BASE+2, SECOND_BASE+2+16, SECOND_BASE, SECOND_BASE+1,
//	(FIRST_BASE)%65,  (FIRST_BASE+1)%65,  (FIRST_BASE+3)%65, (FIRST_BASE+3+16)%65, (FIRST_BASE+4)%65, (FIRST_BASE+4+4)%65, (FIRST_BASE+3+3)%65, (FIRST_BASE+3+1+3)%65};
char *buf = "Hello, World!\x55\xaa";
char outbuf[16];

/* Oscillator setup variants (maybe more), values inside of chip ANDed to not allow by programming errors work it at higher speeds  */
/* WARNING! no chip temperature control limits, etc. It may self-fry and make fried chips with great ease :-) So if trying to overclock */
/* Do not place chip near flammable objects, provide adequate power protection and better wear eye protection ! */
/* Thermal runaway in this case could produce nice flames of chippy fries */

// Thermometer code from left to right - more ones ==> faster clock!

/* Test vectors to calculate (using address-translated loads) */
unsigned atrvec[] = {
0xb0e72d8e, 0x1dc5b862, 0xe9e7c4a6, 0x3050f1f5, 0x8a1a6b7e, 0x7ec384e8, 0x42c1c3fc, 0x8ed158a1, /* MIDSTATE */
0,0,0,0,0,0,0,0,
0x8a0bb7b7, 0x33af304f, 0x0b290c1a, 0xf0c4e61f, /* WDATA: hashMerleRoot[7], nTime, nBits, nNonce */

0x9c4dfdc0, 0xf055c9e1, 0xe60f079d, 0xeeada6da, 0xd459883d, 0xd8049a9d, 0xd49f9a96, 0x15972fed, /* MIDSTATE */
0,0,0,0,0,0,0,0,
0x048b2528, 0x7acb2d4f, 0x0b290c1a, 0xbe00084a, /* WDATA: hashMerleRoot[7], nTime, nBits, nNonce */

0x0317b3ea, 0x1d227d06, 0x3cca281e, 0xa6d0b9da, 0x1a359fe2, 0xa7287e27, 0x8b79c296, 0xc4d88274, /* MIDSTATE */
0,0,0,0,0,0,0,0,
0x328bcd4f, 0x75462d4f, 0x0b290c1a, 0x002c6dbc, /* WDATA: hashMerleRoot[7], nTime, nBits, nNonce */

0xac4e38b6, 0xba0e3b3b, 0x649ad6f8, 0xf72e4c02, 0x93be06fb, 0x366d1126, 0xf4aae554, 0x4ff19c5b, /* MIDSTATE */
0,0,0,0,0,0,0,0,
0x72698140, 0x3bd62b4f, 0x3fd40c1a, 0x801e43e9, /* WDATA: hashMerleRoot[7], nTime, nBits, nNonce */

0x9dbf91c9, 0x12e5066c, 0xf4184b87, 0x8060bc4d, 0x18f9c115, 0xf589d551, 0x0f7f18ae, 0x885aca59, /* MIDSTATE */
0,0,0,0,0,0,0,0,
0x6f3806c3, 0x41f82a4f, 0x3fd40c1a, 0x00334b39, /* WDATA: hashMerleRoot[7], nTime, nBits, nNonce */

};

#define rotrFixed(x,y) (((x) >> (y)) | ((x) << (32-(y))))
#define s0(x) (rotrFixed(x,7)^rotrFixed(x,18)^(x>>3))
#define s1(x) (rotrFixed(x,17)^rotrFixed(x,19)^(x>>10))
#define Ch(x,y,z) (z^(x&(y^z)))
#define Maj(x,y,z) (y^((x^y)&(y^z)))
#define S0(x) (rotrFixed(x,2)^rotrFixed(x,13)^rotrFixed(x,22))
#define S1(x) (rotrFixed(x,6)^rotrFixed(x,11)^rotrFixed(x,25))

/* SHA256 CONSTANTS */
static const unsigned SHA_K[64] = {
        0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
        0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
        0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
        0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
        0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
        0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
        0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
        0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
};

void t_print(struct timespec d_time) {
	printf(" %ds %.2fms\n", (int)d_time.tv_sec, (double)d_time.tv_nsec / 1000000.0);
}



struct  timespec t_add(struct  timespec  time1, struct  timespec  time2) {
    struct  timespec  result ;

    result.tv_sec = time1.tv_sec + time2.tv_sec ;
    result.tv_nsec = time1.tv_nsec + time2.tv_nsec ;
    if (result.tv_nsec >= 1000000000L) {
        result.tv_sec++ ;  result.tv_nsec = result.tv_nsec - 1000000000L ;
    }

    return (result) ;
}



struct timespec t_diff(struct timespec start, struct timespec end)
{
	struct timespec temp;
	if (end.tv_nsec < start.tv_nsec) {
		temp.tv_sec = end.tv_sec-start.tv_sec-1;
		temp.tv_nsec = 1000000000LU;
		temp.tv_nsec -= start.tv_nsec;
		temp.tv_nsec += end.tv_nsec;
	} else {
		temp.tv_sec = end.tv_sec-start.tv_sec;
		temp.tv_nsec = end.tv_nsec-start.tv_nsec;
	}
	return temp;
}

void ms3_compute(unsigned *p)
{
	unsigned a,b,c,d,e,f,g,h, ne, na,  i;

	a = p[0]; b = p[1]; c = p[2]; d = p[3]; e = p[4]; f = p[5]; g = p[6]; h = p[7];

	for (i = 0; i < 3; i++) {
		ne = p[i+16] + SHA_K[i] + h + Ch(e,f,g) + S1(e) + d;
		na = p[i+16] + SHA_K[i] + h + Ch(e,f,g) + S1(e) + S0(a) + Maj(a,b,c);
		d = c; c = b; b = a; a = na;
		h = g; g = f; f = e; e = ne;
	}

	p[15] = a; p[14] = b; p[13] = c; p[12] = d; p[11] = e; p[10] = f; p[9] = g; p[8] = h;
}

void send_conf() {
	config_reg(7,0); config_reg(8,0); config_reg(9,0); config_reg(10,0); config_reg(11,0);
	config_reg(6,0); /* disable OUTSLK */
	config_reg(4,1); /* Enable slow oscillator */
	config_reg(1,0); config_reg(2,0); config_reg(3,0);
	spi_emit_data(0x0100, (void*)counters, 16); /* Program counters correctly for rounds processing, here baby should start consuming power */
}

void send_init() {
	/* Prepare internal buffers */
	/* PREPARE BUFFERS (INITIAL PROGRAMMING) */
	unsigned w[16];
	unsigned atrvec[] = {
		0xb0e72d8e, 0x1dc5b862, 0xe9e7c4a6, 0x3050f1f5, 0x8a1a6b7e, 0x7ec384e8, 0x42c1c3fc, 0x8ed158a1, /* MIDSTATE */
		0,0,0,0,0,0,0,0,
		0x8a0bb7b7, 0x33af304f, 0x0b290c1a, 0xf0c4e61f, /* WDATA: hashMerleRoot[7], nTime, nBits, nNonce */
	};

	ms3_compute(&atrvec[0]);
	memset(&w, 0, sizeof(w)); w[3] = 0xffffffff; w[4] = 0x80000000; w[15] = 0x00000280;
	spi_emit_data(0x1000, (void*)w, 16*4);
	spi_emit_data(0x1400, (void*)w,  8*4);
	memset(w, 0, sizeof(w)); w[0] = 0x80000000; w[7] = 0x100;
	spi_emit_data(0x1900, (void*)&w[0],8*4); /* Prepare MS and W buffers! */
	spi_emit_data(0x3000, (void*)&atrvec[0], 19*4);
}

void set_freq(int bits) {
	uint64_t freq;
	unsigned char *osc6;
	int i;

	osc6 = (unsigned char *)&freq;
	freq = (1ULL << bits) - 1ULL;

	spi_emit_data(0x6000, (void*)osc6, 8); /* Program internal on-die slow oscillator frequency */
	config_reg(4,1); /* Enable slow oscillator */
}

void send_reinit(int slot, int chip_n, int n) {
	spi_clear_buf();
	spi_emit_break();
	spi_emit_fasync(chip_n);
	set_freq(n);
	send_conf();
	send_init();
	tm_i2c_set_oe(slot);
	spi_txrx(spi_gettxbuf(), spi_getrxbuf(), spi_getbufsz());
	tm_i2c_clear_oe(slot);
}

void send_shutdown(int slot, int chip_n) {
	spi_clear_buf();
	spi_emit_break();
	spi_emit_fasync(chip_n);
	config_reg(4,0); /* Disable slow oscillator */
	tm_i2c_set_oe(slot);
	spi_txrx(spi_gettxbuf(), spi_getrxbuf(), spi_getbufsz());
	tm_i2c_clear_oe(slot);
}

void send_freq(int slot, int chip_n, int bits) {
	spi_clear_buf();
	spi_emit_break();
	spi_emit_fasync(chip_n);
	set_freq(bits);
	tm_i2c_set_oe(slot);
	spi_txrx(spi_gettxbuf(), spi_getrxbuf(), spi_getbufsz());
	tm_i2c_clear_oe(slot);
}

unsigned int c_diff(unsigned ocounter, unsigned counter) {
	return counter >  ocounter ? counter - ocounter : (0x003FFFFF - ocounter) + counter;
}

int get_counter(unsigned int *newbuf, unsigned int *oldbuf) {
	int j;
	unsigned counter;
	for(j = 0; j < 16; j++) {
		if (newbuf[j] != oldbuf[j]) {
			int counter = decnonce(newbuf[j]);
			if ((counter & 0xFFC00000) == 0xdf800000) {
				counter -= 0xdf800000;
				return counter;
			}
		}
	}
	return 0;
}

int get_diff(unsigned int *newbuf, unsigned int *oldbuf) {
		int j;
		unsigned counter = 0;
		for(j = 0; j < 16; j++) {
				if (newbuf[j] != oldbuf[j]) {
						counter++;
				}
		}
		return counter;
}

int detect_chip(int chip_n) {
	int i;
	unsigned newbuf[17], oldbuf[17];
	unsigned ocounter;
	int odiff;
	struct timespec t1, t2, td;

	memset(newbuf, 0, 17 * 4);
	memset(oldbuf, 0, 17 * 4);


	spi_clear_buf();
	spi_emit_break(); /* First we want to break chain! Otherwise we'll get all of traffic bounced to output */
	spi_emit_fasync(chip_n);
	set_freq(52);  //54 - 3F, 53 - 1F
	send_conf();
	send_init();
	spi_txrx(spi_gettxbuf(), spi_getrxbuf(), spi_getbufsz());

	ocounter = 0;
	for (i = 0; i < BITFURY_DETECT_TRIES; i++) {
		int j;
		int counter;

		spi_clear_buf();
		spi_emit_break();
		spi_emit_fasync(chip_n);
		spi_emit_data(0x3000, (void*)&atrvec[0], 19*4);
		spi_txrx(spi_gettxbuf(), spi_getrxbuf(), spi_getbufsz());
		memcpy(newbuf, spi_getrxbuf() + 4 + chip_n, 17*4);

		clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t1);
		counter = get_counter(newbuf, oldbuf);
		if (ocounter) {
			unsigned int cdiff = c_diff(ocounter, counter);
			unsigned per_ms;

			td = t_diff(t2, t1);
			per_ms = cdiff / (td.tv_nsec / 1000);
			if (cdiff > 5000 && cdiff < 100000 && odiff > 5000 && odiff < 100000)
				return 1;
			odiff = cdiff;
		}
		ocounter = counter;
		t2 = t1;
		if (newbuf[16] != 0 && newbuf[16] != 0xFFFFFFFF) {
			return 0;
		}
        nmsleep(BITFURY_REFRESH_DELAY / 10);
		memcpy(oldbuf, newbuf, 17 * 4);
	}
	return 0;
}

int libbitfury_detectChips(struct bitfury_device *devices) {
	int n = 0;
	int i;
	static int slot_on[BITFURY_MAXBANKS];
	struct timespec t1, t2;

	if (tm_i2c_init() < 0) {
		printf("I2C init error\n");
		return(1);
	}

	ms3_compute(&atrvec[0]);
	ms3_compute(&atrvec[20]);
	ms3_compute(&atrvec[40]);
	spi_init();

	for (i = 0; i < BITFURY_MAXBANKS; i++) {
		slot_on[i] = 0;
	}

	clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t1);
	for (i = 0; i < BITFURY_MAXBANKS; i++) {
		int slot_detected = tm_i2c_detect(i) != -1;
		slot_on[i] = slot_detected;
		tm_i2c_clear_oe(i);
        nmsleep(1);
	}

    for (i = 0; i < BITFURY_MAXBANKS; i++) {
//		if (slot_on[i]) {
            int chip_index = 0;
            int cnt_on_slot = 0;
            int chip_detected = 0;

            char num_chip[16] = { 0 };
            char slot_line [1024] = { 0 };

            tm_i2c_set_oe(i);
            do {
                strcpy (num_chip, "   ");
                chip_detected = detect_chip(chip_index);

                if (!chip_detected)
                {
                    if (cnt_on_slot) applog(LOG_WARNING, "BITFURY slot: 0x%02X, chip #%X not detected !!!", i, n);
                    chip_detected = detect_chip(chip_index);
                }

                if (chip_detected) {
                    // applog(LOG_WARNING, "BITFURY slot: 0x%02X, chip #%X detected", i, n);
                    snprintf ( num_chip, 15, "%02X ", chip_index );
                    devices[n].slot = i;
                    devices[n].fasync = chip_index;
                    n++;
                    cnt_on_slot ++;
                }
                else
                    if (!chip_index) break; // если нулевой чип не найден, то и остальные искать бесполезно на практике

                strncat(slot_line, num_chip, 1023);
                chip_index++;
            } while (chip_index < BITFURY_BANKCHIPS);

            if (cnt_on_slot) applog(LOG_WARNING, "BITFURY slot 0x%02X, chips detected: %s = %d", i, slot_line, cnt_on_slot);

#ifdef      BITFURY_METABANK
            if (cnt_on_slot && cnt_on_slot < BITFURY_BANKCHIPS) {
                sprintf(slot_line, "For slot %X detected only %d chips from %d", i, cnt_on_slot, BITFURY_BANKCHIPS);
                quit(1, slot_line);
            }
#endif

            tm_i2c_clear_oe(i);
//		}
    }

	clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t2);

	return n;
}

int libbitfury_shutdownChips(struct bitfury_device *devices, int chip_n) {
	int i;
	for (i = 0; i < chip_n; i++) {
		send_shutdown(devices[i].slot, devices[i].fasync);
	}
	tm_i2c_close();
}

unsigned decnonce(unsigned in)
{
	unsigned out;

	/* First part load */
	out = (in & 0xFF) << 24; in >>= 8;

	/* Byte reversal */
	in = (((in & 0xaaaaaaaa) >> 1) | ((in & 0x55555555) << 1));
	in = (((in & 0xcccccccc) >> 2) | ((in & 0x33333333) << 2));
	in = (((in & 0xf0f0f0f0) >> 4) | ((in & 0x0f0f0f0f) << 4));

	out |= (in >> 2)&0x3FFFFF;

	/* Extraction */
	if (in & 1) out |= (1 << 23);
	if (in & 2) out |= (1 << 22);

	out -= 0x800004;
	return out;
}

int rehash(const void *midstate, const uint32_t m7, const uint32_t ntime, const uint32_t nbits, uint32_t nnonce) {
	unsigned char in[16];
	unsigned int *in32 = (unsigned int *)in;
	unsigned int *mid32 = (unsigned int *)midstate;
	unsigned out32[8];
	unsigned char *out = (unsigned char *) out32;
#ifdef BITFURY_REHASH_DEBUG
	static unsigned history[512];
	static unsigned history_p;
#endif


	nnonce = bswap_32(nnonce);
	in32[0] = bswap_32(m7);
	in32[1] = bswap_32(ntime);
	in32[2] = bswap_32(nbits);
	in32[3] = nnonce;
#ifdef BFGMINER_MOD
    sha256_ctx ctx;
    memset( &ctx, 0, sizeof( sha256_ctx ) );
    memcpy(ctx.h, mid32, 8*4);
    ctx.tot_len = 64;
    ctx.len = 0;

	sha256_update(&ctx, in, 16);
	sha256_final(&ctx, out);
	sha256(out, 32, out);
#else
    sha2_context ctx;
    memset( &ctx, 0, sizeof( sha2_context ) );
    memcpy(ctx.state, mid32, 8*4);
    ctx.total[0] = 64;
    ctx.total[1] = 0;
    sha2_update(&ctx, in, 16);
    sha2_finish(&ctx, out);
    sha2(out, 32, out);
#endif


	if (out32[7] == 0) {
#ifdef BITFURY_REHASH_DEBUG
		char hex[65];
        char *r = bin2hex(hex, out, 32);
		applog(LOG_INFO, "! MS0: %08x, m7: %08x, ntime: %08x, nbits: %08x, nnonce: %08x", mid32[0], m7, ntime, nbits, nnonce);
		applog(LOG_INFO, " out: %s", hex);
        cfree (r);
		history[history_p] = nnonce;
		history_p++; history_p &= 512 - 1;
#endif
		return 1;
	}
	return 0;
}

void work_to_payload(struct bitfury_payload *p, struct work *w) {
	unsigned char flipped_data[80];

	memset(p, 0, sizeof(struct bitfury_payload));
#ifdef BFGMINER_MOD
	swap32yes(flipped_data, w->data, 80 / 4);
#else
    flip80(flipped_data, w->data);
#endif
	memcpy(p->midstate, w->midstate, 32);
	p->m7 = bswap_32(*(unsigned *)(flipped_data + 64));
	p->ntime = bswap_32(*(unsigned *)(flipped_data + 68));
	p->nbits = bswap_32(*(unsigned *)(flipped_data + 72));
}

inline unsigned rehash_test(struct bitfury_device *d, struct bitfury_payload *op, unsigned pn, unsigned offset) {

    pn += offset;
    bool result = rehash(op->midstate, op->m7, op->ntime, op->nbits, pn);

    if ( result && offset && 0 == offset & 0x80000000 ) {
        applog(LOG_WARNING, "found solution for chip %X:%X, offset = 0x%X ", d->fasync, d->slot, offset);
    }

    return result ? pn : 0;

}

int test_result(unsigned int *results, int *counter, const unsigned int value, int *dups) {
    int i, dcnt = 0, cnt = counter[0];


    for (i = 0; i < cnt; i ++)
        if (results [i] == value) dcnt++;

    dups[0] = dcnt;
    if (dcnt) return 0; // have duplicates

    results[cnt] = value;
    counter[0] ++;
    return 1;
}


void libbitfury_sendHashOne(struct thr_info *thr, struct bitfury_device *d) {

    unsigned *newbuf = d->newbuf;
    unsigned *oldbuf = d->oldbuf;
    struct bitfury_payload   *p = &(d->payload);
    struct bitfury_payload  *op = &(d->opayload);
    struct bitfury_payload *o2p = &(d->o2payload);
    struct timespec d_time;
    struct timespec time;

#define TEST_OFFSETS 3
    unsigned offsets [6] = { 0, 0xFF800000, 0xFFC00000, 0x02800000, 0x02C00000, 0x00400000 };

    int i;
    int chip = d->fasync;
    int slot = d->slot;

    char *txbuff = NULL;
    char *rxbuff = NULL;

    clock_gettime(CLOCK_REALTIME, &(time));

    if(1) {
        int i;
        int results_num = d->results_n;
        int found = 0;
        unsigned * results = d->results;
        tm_i2c_set_oe(slot);
        // nusleep(100);
        // prepare work
        memcpy(atrvec, p, 20 * 4);
        ms3_compute(atrvec);

        /* Programming next value */

        if (1) {
            spi_clear_buf(); spi_emit_break();
            spi_emit_fasync(chip);
            spi_emit_data(0x3000, (void*)&atrvec[0], 19*4);
        }

        txbuff = spi_gettxbuf();
        rxbuff = spi_getrxbuf();

        spi_txrx(txbuff, rxbuff, spi_getbufsz());
        memcpy(newbuf, rxbuff + 4 + chip, 17 * 4); // 16 x nonce + flags (?) + chip

        tm_i2c_clear_oe(slot);
        d->job_switched = ( newbuf[16] != oldbuf[16] );

        d->old_num = 0;
        d->future_num = 0;

        int dups = 0;
        if (op && o2p && d->job_switched)
          for (i = 0; i < 16; i++) {
            if (oldbuf[i] != newbuf[i] ) {
                int n, pr;
                unsigned pn;        // possible nonce
                unsigned int s = 0; // TODO zero may be solution
                unsigned int so = 0;
                unsigned int sf = 0;
                unsigned int fl = 0;
                if ( (newbuf[i] & 0xFF) == 0xE0 ) continue;


                pn = decnonce(newbuf[i]);
                pr = results_num;

                // oldbuf[i] = newbuf [i]; // prevent next reaction

                for (n = 0; n < TEST_OFFSETS; n ++) {
                    if (!s)  s  = rehash_test(d,  op, pn, offsets[n] );
                    if (!so) so = rehash_test(d, o2p, pn, offsets[n] );
                    if (!sf) sf = rehash_test(d,   p, pn, offsets[n] );
                }


                if (s) {
                    s = bswap_32(s);
                    fl += test_result(results, &results_num, s, &dups);
                }


                if (so) {
                    so = bswap_32(so);
                    fl += test_result(d->old_nonce, &d->old_num, so, &dups);
                }

                if (sf) {
                    sf = bswap_32(sf);
                    fl += test_result(d->future_nonce, &d->future_num, sf, &dups);
                }

                found += fl;
                d->found_last += fl;

                int wrk_id = 0;
                if ( d->work )
                     wrk_id = d->work->id;

                if ( d->found_last > 50 && fl > 0 )
                    applog(LOG_WARNING, "#DBG: chip %X_%X[%X] old = %08X, new = %08X, s = { %08X, %08X:%X->%X, %08X }, work.id = %3d, fl = %d, ft = %3d, dups = %d ",
                                               d->fasync, d->slot, i, oldbuf[i], newbuf[i], so, s, pr, results_num, sf, wrk_id, fl, d->found_last, dups ); // */


                if ( dups && 15 == i )
                     d->job_switched = 1;
                /*
                if(     rehash(op->midstate, op->m7, op->ntime, op->nbits, pn)) s = pn;
                else if(rehash(op->midstate, op->m7, op->ntime, op->nbits, pn-0x00400000)) s = pn - 0x00400000;
                else if(rehash(op->midstate, op->m7, op->ntime, op->nbits, pn-0x00800000)) s = pn - 0x00800000;
                else if(rehash(op->midstate, op->m7, op->ntime, op->nbits, pn+0x02800000)) s = pn + 0x02800000;
                else if(rehash(op->midstate, op->m7, op->ntime, op->nbits, pn+0x02C00000)) s = pn + 0x02C00000;
                else if(rehash(op->midstate, op->m7, op->ntime, op->nbits, pn+0x00400000)) s = pn + 0x00400000;
                if (s) {
                    results[results_num++] = bswap_32(s);
                    found++;
                }

                s = 0;
                if (    rehash(o2p->midstate, o2p->m7, o2p->ntime, o2p->nbits, pn)) s = pn;
                else if(rehash(o2p->midstate, o2p->m7, o2p->ntime, o2p->nbits, pn-0x00400000)) s = pn - 0x00400000;
                else if(rehash(o2p->midstate, o2p->m7, o2p->ntime, o2p->nbits, pn-0x00800000)) s = pn - 0x00800000;
                else if(rehash(o2p->midstate, o2p->m7, o2p->ntime, o2p->nbits, pn+0x02800000)) s = pn + 0x02800000;
                else if(rehash(o2p->midstate, o2p->m7, o2p->ntime, o2p->nbits, pn+0x02C00000)) s = pn + 0x02C00000;
                else if(rehash(o2p->midstate, o2p->m7, o2p->ntime, o2p->nbits, pn+0x00400000)) s = pn + 0x00400000;
                if (s) {
                    d->old_nonce[d->old_num++] = bswap_32(s);
                    found++;
                }

                s = 0;
                if(     rehash(p->midstate, p->m7, p->ntime, p->nbits, pn)) s = pn;
                else if(rehash(p->midstate, p->m7, p->ntime, p->nbits, pn-0x00400000)) s = pn - 0x00400000;
                else if(rehash(p->midstate, p->m7, p->ntime, p->nbits, pn-0x00800000)) s = pn - 0x00800000;
                else if(rehash(p->midstate, p->m7, p->ntime, p->nbits, pn+0x02800000)) s = pn + 0x02800000;
                else if(rehash(p->midstate, p->m7, p->ntime, p->nbits, pn+0x02C00000)) s = pn + 0x02C00000;
                else if(rehash(p->midstate, p->m7, p->ntime, p->nbits, pn+0x00400000)) s = pn + 0x00400000;
                if (s) {
                    d->future_nonce[d->future_num++] = bswap_32(s);
                    found++;
                }
                // */
                // сразу после переключения частоты ошибки не засчитывать, т.к. бывает иногда много
                if (fl < 1 && 0 == dups && d->csw_back > 1) {
                    d->hw_errors++;
#ifdef BFGMINER_MOD
                    inc_hw_errors2(thr, NULL, &pn);
#else
                    inc_hw_errors(thr);
#endif
                }
            }
        }
        d->results_n = results_num;

        if (d->job_switched) {
            memcpy(o2p, op, sizeof(struct bitfury_payload));
            memcpy(op,  p,  sizeof(struct bitfury_payload));
            memcpy(oldbuf, newbuf, 17 * 4);
        }
    }
}



void libbitfury_sendHashData(struct thr_info *thr, struct bitfury_device *bf, int chip_count) {
	int chip_id;

	static unsigned second_run;

//	clock_gettime(CLOCK_REALTIME, &(time));

    for (chip_id = 0; chip_id < chip_count; chip_id++)
        libbitfury_sendHashOne (thr, &bf[chip_id]);
	second_run = 1;
	return;
}

int libbitfury_readHashData(unsigned int *res) {
	return 0;
}

