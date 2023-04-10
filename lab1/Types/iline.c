#include "iline.h"

ILine ILine_New(Float1 x1, Float1 y1, Float1 x2, Float1 y2) {
    ILine ret = {
        .header.size = sizeof(Float4),
        .header.type_id = ITEM_LINE_TYPE_ID,
        .start.x = x1,
        .start.y = y1,
        .end.x = x2,
        .end.y = y2,
    };

    return ret;
}

ILine ILine_FromFloat4(Float4 f) {
    return ILine_New(f.x1, f.y1, f.x2, f.y2);
}
