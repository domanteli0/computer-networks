#include <stdint.h>
#include "floatx.h"

#define ITEM_DOT_TYPE_ID 0x0000

typedef struct __attribute__((packed)) IDotData {
    uint32_t type_id;
    Float1 x;
    Float1 y;
} IDotData;

char *IDotData_ToString(IDotData this) {
    char *str = calloc(1024, 1);
    sprintf(str, "Type: 0x%x, x: %f, y: %f", this.type_id, this.x, this.y);
    return str;
}

IDotData IDotData_FromFloat2(Float2 f2) {
    IDotData ret = {
        .type_id = ITEM_DOT_TYPE_ID,
        .x = f2.x,
        .y = f2.y,
    };

    return ret;
}

IDotData IDotData_New(Float1 x, Float1 y) {
    IDotData ret = {
        .type_id = ITEM_DOT_TYPE_ID,
        .x = x,
        .y = y,
    };

    return ret;
}

