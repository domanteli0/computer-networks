#pragma once

#include <inttypes.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

typedef struct BitArr {
    uint64_t data[4];
} BitArr;

BitArr BitArr_New();
void BitArr_Flip(BitArr *, uint8_t);
void BitArr_FlipU(BitArr *, size_t);

bool BitArr_Get(BitArr, uint8_t);
bool BitArr_GetU(BitArr, size_t);

void BitArr_Clear(BitArr *, uint8_t);

void BitArr_Set(BitArr *, uint8_t);
