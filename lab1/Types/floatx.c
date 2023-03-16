#include "floatx.h"

bool Float2_EqualsPtr(Float2 *left, Float2 *right) {
    assert(left != NULL);
    assert(right != NULL);

    bool x = left->x == right->x;
    bool y = left->y == right->y;

    return x && y;
}