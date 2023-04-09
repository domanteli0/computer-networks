#include "floatx.h"

bool Float2_EqualsPtr(Float2 *left, Float2 *right) {
    assert(left != NULL);
    assert(right != NULL);

    bool x = left->x == right->x;
    bool y = left->y == right->y;

    return x && y;
}

Float2 Float2_New(float x, float y) {
    Float2 ret = {
        .x = x,
        .y = y,
    };

    return ret;
}

// Creates a new Float2 on the heap
// the caller must free the float2  
Float2 *Float2_NewPtr(float f1, float f2) {
    Float2 *ptr = calloc(1, sizeof(Float2));

    memcpy(&ptr->x, &f1, sizeof(float));
    memcpy(&ptr->y, &f2, sizeof(float));
    
    return ptr;
}

// Stringifies a `Float2`.
// The caller is responsible for freeing the string with `free()` 
char *Float2_ToString(Float2 this) {
    const size_t MAX_STR_LEN = 128;
    char *str = calloc(MAX_STR_LEN, sizeof(char));

    sprintf(str, "(%f, %f)", this.x, this.y);
    return str;
}
