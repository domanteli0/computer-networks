#include "../Types/header.h"
#include "../Types/idot.h"
#include "../Types/iline.h"
#include "../Types/floatx.h"
#include "../DynArr/dynarr.h"
#include <string.h>
#include <stdio.h>

void DynArr_test() {
    DynArr arr = DynArr_WithCapacity(0, sizeof(int));
    int temp = -1;
    DynArr_Push(&arr, &temp);
    DynArr_Push(&arr, &temp);
    temp = 0;
    DynArr_Push(&arr, &temp);
    temp = -1;
    DynArr_Push(&arr, &temp);

    for (int ix = 1; ix < 10; ++ix) {
        DynArr_Push(&arr, &ix);
        DynArr_Push(&arr, &temp);
    }

    DynArr_Push(&arr, &temp);
    DynArr_Push(&arr, &temp);

    DynArr_FilterOut(&arr, &temp);

    for (int ix = 0; ix < 10; ++ix) {
        assert(ix == ((int *) arr.arr_ptr)[ix]);
    }
}

void IDotData_test() {
    // These tests make sure that the assumed struct layout in memory is correct:
    // | Header [4 bytes] | Data [8 bytes] |

    assert(sizeof(DrawableHeader) == 4);
    assert(sizeof(Float2) == 8);
    assert(sizeof(IDotData) == 12);

    DrawableHeader header = {
        .size = 12,
        .type_id = 0x0,
    };

    IDotData idot = {
        .header = header,
        .x = 1,
        .y = 2,
    };

    Float2 f2 = {
        .x = 1,
        .y = 2,
    };

    assert(
        memcmp(&header, &idot, sizeof(DrawableHeader)) == 0
    );
    assert(
        memcmp(&f2, ((DrawableHeader *) &idot ) + 1, sizeof(Float2)) == 0
    );

    assert(&(idot.header.type_id) == &(idot));
    assert(&(idot.x) == (((uint32_t *) &idot) + 1));
    assert(&(idot.y) == (((uint32_t *) &idot) + 2));
}

void ILine_test() {
    assert(sizeof(ILine) == sizeof(DrawableHeader) + sizeof(Float4));
    assert(sizeof(ILine) == sizeof(DrawableHeader) + (sizeof(Float2) * 2));
    assert(sizeof(Float2) * 2 == sizeof(Float4));

    Float4 f4 = Float4_New(
        10,
        -1e10,
        2e-5,
        5e27
    );

    ILine line = ILine_FromFloat4(f4);

    DrawableHeader header = {
        .size = sizeof(Float4),
        .type_id = ITEM_LINE_TYPE_ID,
    };

    assert(
        memcmp( &header, &line, sizeof(DrawableHeader)) == 0
    );

    assert(
        memcmp(
            ((DrawableHeader *) &line) + 1,
            &f4,
            sizeof(Float4)
        ) == 0
    );
}

int main() {
    IDotData_test();
    DynArr_test();
    ILine_test();

    assert(sizeof(uint32_t) == sizeof(Float1));
    
    printf("All tests passed 🥳\n");
}

