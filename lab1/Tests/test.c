#include "../Types/header.h"
#include "../Types/idot.h"
#include "../Types/floatx.h"
#include <string.h>
#include <stdio.h>

int main() {
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

    printf("All tests passed 🥳\n");
}
