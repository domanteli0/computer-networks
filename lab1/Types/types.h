#pragma once

#include "idot.h"
#include "iline.h"

#define MAX_SIZEOF(left, right) sizeof(left) > sizeof(right) ? sizeof(left) : sizeof(right)

#define MAX_TYPE_SIZE MAX_SIZEOF(IDotData, ILine)
