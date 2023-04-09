#pragma once
#include <stdint.h>

typedef struct DrawableHeader {
    uint16_t type_id;
    uint16_t size;
} DrawableHeader;
