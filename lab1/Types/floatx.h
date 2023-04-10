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

typedef struct Float4 {
    Float1 x1;
    Float1 y1;
    Float1 x2;
    Float1 y2;
} Float4;

bool Float2_EqualsPtr(Float2 *left, Float2 *right);
char *Float2_ToString(Float2 this);
Float2 Float2_New(float x, float y);
Float2 *Float2_NewPtr(float f1, float f2);
Float4 Float4_New(Float1 x1, Float1 y1, Float1 x2, Float1 y2);
Float4 Float4_FromFloat4(Float4 f4);
void Float4_Print(Float4 this);
