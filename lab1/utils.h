#pragma once

#include <stdint.h>
#include <sys/time.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <time.h>

struct RngGenPrivate;
typedef struct RngGenPrivate *RngGen; 

uint64_t timestamp_utc();
uint64_t RngGen_64bits(RngGen rng_gen);
RngGen RngGen_new();
void RngGen_free(RngGen rng_gen);
struct timeval timeval_FromMicro(int micro_seconds);