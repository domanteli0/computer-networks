#pragma once

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>
#include <stdbool.h>

// Dynamicaly resized array
typedef struct DynArr {
    void *arr_ptr;
    size_t capacity;     // counted in terms of elements, not the individual bytes
    size_t size;         // :ditto
    size_t size_of_elem; // size of individual elements
} DynArr;

DynArr DynArr_WithCapacity(size_t capacity, size_t size_of_elems);
void *DynArr_Get(DynArr this, size_t pos);
void DynArr_Push(DynArr *this, void *elem);
void DynArr_Free(DynArr this);

#define DynArr_ForEach(dyn_arr, type, elem, action) \
{ \
    void *DYNARR_RESERVED_ADDR = dyn_arr.arr_ptr;\
    for (size_t DYNARR_RESERVED_INDEX = 0; DYNARR_RESERVED_INDEX < (dyn_arr).size; ++DYNARR_RESERVED_INDEX) { \
        type *(elem) = DYNARR_RESERVED_ADDR; \
        {\
            action \
        }\
        DYNARR_RESERVED_ADDR += (dyn_arr).size_of_elem;\
    }\
}
