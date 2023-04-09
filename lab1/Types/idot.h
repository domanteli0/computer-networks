#pragma once
#include <stdint.h>
#include <inttypes.h>
#include "header.h"
#include "floatx.h"
#include <stdio.h>

#define ITEM_DOT_TYPE_ID 0x0000

typedef struct IDotData {
    DrawableHeader header;
    Float1 x;
    Float1 y;
} IDotData;

char *IDotData_ToString(IDotData this);
IDotData IDotData_FromFloat2(Float2 f2);
IDotData IDotData_New(Float1 x, Float1 y);
