#pragma once

#include <stdbool.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <string.h>

#define Float1 float

typedef struct Float2 {
    Float1 x;
    Float1 y;
} Float2;

bool Float2_EqualsPtr(Float2 *left, Float2 *right);
char *Float2_ToString(Float2 this);
Float2 Float2_New(float x, float y);
Float2 *Float2_NewPtr(float f1, float f2);
