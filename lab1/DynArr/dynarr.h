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
void *DynArr_FindWithPredicate(DynArr this, bool(pred)(void *));
void *DynArr_Find(DynArr this, void *elem);
// void DynArr_Filter(DynArr this, bool(pred)(void *));
void DynArr_FilterOut(DynArr *this, void *elem_cmp);

#define DynArr_ForEach(dyn_arr, type, elem, action) \
{ \
    for (size_t DYNARR_RESERVED_INDEX = 0; DYNARR_RESERVED_INDEX < (dyn_arr).size; ++DYNARR_RESERVED_INDEX) { \
        void *DYNARR_RESERVED_ADDR = (dyn_arr).arr_ptr + (DYNARR_RESERVED_INDEX * (dyn_arr).size_of_elem); \
        type elem = *((type *) DYNARR_RESERVED_ADDR); \
        {\
            action \
        }\
    }\
}

#define DynArr_ForEachPtr(dyn_arr, elem, action) \
{ \
    for (size_t DYNARR_RESERVED_INDEX = 0; DYNARR_RESERVED_INDEX < (dyn_arr).size; ++DYNARR_RESERVED_INDEX) { \
        void *elem = (dyn_arr).arr_ptr + (DYNARR_RESERVED_INDEX * (dyn_arr).size_of_elem); \
        {\
            action \
        }\
    }\
}

