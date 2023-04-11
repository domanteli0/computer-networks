#pragma once
#include <stdint.h>
#include <stdio.h>
#include <inttypes.h>

typedef struct DrawableHeader {
    uint16_t type_id;
    uint16_t size;
} DrawableHeader;

void DrawableHeader_Printf(DrawableHeader this);