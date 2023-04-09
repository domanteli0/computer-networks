#pragma once

#include <stdint.h>
#include <sys/time.h>
#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <time.h>
#include <string.h>
#include <stdbool.h>

#include <sys/socket.h>
#include <unistd.h>

#define TCP_RECV(fd, data, size, flags) if (TCP_Recv(fd, data, size, flags) < 0) { continue; }

struct RngGenPrivate;
typedef struct RngGenPrivate *RngGen; 

uint64_t timestamp_utc();
uint64_t RngGen_64bits(RngGen rng_gen);
RngGen RngGen_new();

void RngGen_free(RngGen rng_gen);
struct timeval timeval_FromMicro(int micro_seconds);
struct fd_set fd_set_Singleton(int fd);
int TCP_Send(int *fd, void *data, size_t size, int flags);
int TCP_Recv(int *fd, void *data, size_t size, int flags);