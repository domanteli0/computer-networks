#include "utils.h"

// returns current time in milis from posix time epoch
uint64_t timestamp_utc() {
    struct timeval time;
    gettimeofday(&time, NULL);
    int64_t s1 = (int64_t)(time.tv_sec) * 1000;
    int64_t s2 = (time.tv_usec / 1000);

    int64_t milis = s1 + s2;
    assert(milis >= 0);

    return milis;
}

uint64_t random_64bits(RandomGenerator rng_gen) {
    uint64_t ret;
    assert(
        fread(
            &ret,
            sizeof(uint64_t),
            1,
            rng_gen->dev_random
        ) == 1
    );

    return ret;
}

struct RngRand {
    FILE *dev_random;
};

RandomGenerator RandomGenerator_new() {
    RandomGenerator ret = calloc(sizeof(struct RngRand), 1);
    ret->dev_random = fopen("/dev/urandom", "rb");

    return ret;
}

void RandomGenerator_free(RandomGenerator rng_gen) {
    fclose(rng_gen->dev_random);
    free(rng_gen);
}
