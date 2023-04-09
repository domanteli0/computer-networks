#include "idot.h"

char *IDotData_ToString(IDotData this) {
    char *str = calloc(1024, 1);
    sprintf(str, "Type: 0x%" PRIx16 ", Size: 0x%" PRIx16 ", x: %f, y: %f", this.header.type_id, this.header.size, this.x, this.y);
    return str;
}

IDotData IDotData_FromFloat2(Float2 f2) {
    IDotData ret = {
        .header.type_id = ITEM_DOT_TYPE_ID,
        .header.size = sizeof(Float2),
        .x = f2.x,
        .y = f2.y,
    };

    return ret;
}

IDotData IDotData_New(Float1 x, Float1 y) {
    IDotData ret = {
        .header.type_id = ITEM_DOT_TYPE_ID,
        .header.size = sizeof(Float2),
        .x = x,
        .y = y,
    };

    return ret;
}

