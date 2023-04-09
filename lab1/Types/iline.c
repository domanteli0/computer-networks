#include "iline.h"

ILine ILine_New(Float1 x1, Float1 y1, Float1 x2, Float1 y2) {
    ILine ret = {
        .header.size = 2 * sizeof(Float2),
        .header.type_id = ITEM_LINE_TYPE_ID,
        .start.x = x1,
        .start.y = y1,
        .end.x = x2,
        .end.y = y2,
    };

    return ret;
}