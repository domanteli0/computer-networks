#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

// Dynamicaly resized array
typedef struct DynArr {
    void *arr_ptr;
    size_t capacity;     // counted in terms of elements, not the individual bytes
    size_t size;         // :ditto
    size_t size_of_elem; // size of individual elements
} DynArr;

// Allocates enough space to meet the desired capacity
// the acutal space allocated may be more than specified
DynArr DynArr_WithCapacityS(size_t capacity, size_t size_of_elems) {
    size_t cap_to_alloc = 1;

    while (cap_to_alloc < capacity) {
       cap_to_alloc *= 2;
    }

    DynArr arr = {
        .arr_ptr = calloc(cap_to_alloc, size_of_elems),
        .capacity = cap_to_alloc,
        .size = 0,
        .size_of_elem = size_of_elems,
    };

    return arr;
}

void *DynArr_Get(DynArr this, size_t pos) {
    if ((pos) >= (this.size)) {
        return NULL;
    }

    return this.arr_ptr + (pos * (this.size_of_elem));
}

// Copies the data into the array
// any pointers pointing to the `elem` will refer to the original
// and not the one in the array 
void DynArr_Push(DynArr *this, void *elem) {
    if (this->capacity <= this->size) {
        this->arr_ptr = realloc(this->arr_ptr, (this->capacity) * 2);
        this->capacity *= 2;

        assert(this->arr_ptr != NULL);
    }

    memcpy(
        (this->arr_ptr) + ((this->size) * (this->size_of_elem)), 
        elem, 
        this->size_of_elem
        );
    this->size += 1;
}

#define DynArr_ForEach(dyn_arr, type, elem, action) \
for (size_t DYNARR_RESERVED_INDEX = 0; DYNARR_RESERVED_INDEX < (dyn_arr).size; ++DYNARR_RESERVED_INDEX) { \
    type *(elem) = (dyn_arr).arr_ptr + (((dyn_arr).size_of_elem) * ix); \
    action \
}

// frees the array
// any data pointed to, by elements in the array,
// must, if needed, be free beforehand
void DynArr_Free(DynArr this) {
    free(this.arr_ptr);
}
