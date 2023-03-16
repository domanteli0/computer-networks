#pragma once

#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>

#define Float1 float

typedef struct Float2 {
    Float1 x;
    Float1 y;
} Float2;

bool Float2_EqualsPtr(Float2 *left, Float2 *right);