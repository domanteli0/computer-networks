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

struct RngGenPrivate {
    FILE *dev_random;
};

uint64_t RngGen_64bits(RngGen rng_gen) {
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

RngGen RngGen_new() {
    RngGen ret = calloc(sizeof(struct RngGenPrivate), 1);
    ret->dev_random = fopen("/dev/urandom", "rb");

    return ret;
}

void RngGen_free(RngGen rng_gen) {
    fclose(rng_gen->dev_random);
    free(rng_gen);
}

struct timeval timeval_FromMicro(int i) {
    struct timeval ret = {
        .tv_sec = 0,
        .tv_usec = i,
    };

    return ret;
} 

struct fd_set fd_set_Singleton(int fd) {
    struct fd_set read_set;
	memset(&read_set, 0, sizeof(fd_set));	
    FD_ZERO(&read_set);
    FD_SET(fd, &read_set);

    return read_set;
}

int SelectOne(int fd, int timeout) {
    struct fd_set read_set = fd_set_Singleton(fd);

    struct timeval timeout_s = timeval_FromMicro(timeout);

    int ret = select(fd + 1, &read_set, NULL, NULL, &timeout_s); 

    return ret;
}

// wraps `send()` calls, sets fd to -1, in case of failure
int TCP_Send(int *fd, void *data, size_t size, int flags) {
    if (*fd == -1)
        return -1;

    // TO ASK: what do kai `send()` grazina tarp 0 ir size 
    int send_len = send(*fd, data, size, flags);
    if (send_len < 0 || send_len < (int) size) {
        fprintf(stderr, "Send of size %zu failed, closing handle %i\n", size, *fd);
        close(*fd);
        *fd = -1;
    }
    
    return send_len;
}

// wraps `recv()` calls, sets fd to -1, in case of failure
int TCP_Recv(int *fd, void *data, size_t size, int flags) {
    if (*fd == -1)
        return -1;
    
    int recv_len = recv(*fd, data, size, flags);
    if (recv_len < 1 || recv_len < (int) size) {
        fprintf(stderr, "Recv of size %zu failed, closing handle: %i\n", size, *fd);
        close(*fd);
        *fd = -1;
    }
    
    return recv_len;
}
