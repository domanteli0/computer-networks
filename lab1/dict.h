#pragma once

#include "dynarr.h"

typedef struct Dict {
    DynArr dyn_arr;
    size_t key_size;
    size_t value_size;
    void *reserved;
} Dict;

Dict Dict_WithCapacity(size_t capacity, 
                       size_t key_size, 
                       size_t value_size); 

void Dict_Insert(Dict *this, void *key, void *value);

void *Dict_Get(Dict this, void *key);

void Dict_Free(Dict this);