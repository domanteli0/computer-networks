#pragma once

#include "header.h"
#include "floatx.h"

#define ITEM_LINE_TYPE_ID 0x0001

typedef struct ILine {
    DrawableHeader header;
    Float2 start;
    Float2 end;
} ILine;

ILine ILine_New(Float1 x1, Float1 y1, Float1 x2, Float1 y2);