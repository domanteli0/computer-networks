#pragma once

#include <stdint.h>
#include <sys/time.h>
#include <stdio.h>

struct RngRand;
typedef struct RngRand *RandomGenerator; 

uint64_t timestamp_utc();
uint64_t random_64bits(RandomGenerator rng_gen);
RandomGenerator RandomGenerator_new();
void RandomGenerator_free(RandomGenerator rng_gen);