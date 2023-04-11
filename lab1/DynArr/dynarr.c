#include "dynarr.h"

// Allocates enough space to meet the desired capacity
// the acutal space allocated may be more than specified
DynArr DynArr_WithCapacity(size_t capacity, size_t size_of_elems) {
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

    assert(arr.arr_ptr != NULL);

    return arr;
}

// Gets the element at pos, return NULL if pos is out of range 
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
    assert(this != NULL);
    assert(elem != NULL);
        
    if (this->capacity <= this->size) {
        this->arr_ptr = realloc(this->arr_ptr, (this->capacity) * 2 * (this->size_of_elem));
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

// frees the array
// any data pointed to, by elements in the array,
// must, if needed, be free beforehand
void DynArr_Free(DynArr this) {
    free(this.arr_ptr);
}

void *DynArr_Find(DynArr this, void *elem) {
    void *current = this.arr_ptr;
    for (size_t ix = 0; ix < this.size; ++ix) {
        current += this.size_of_elem; 
        if (memcmp(current, elem, this.size_of_elem) == 0) {
            return current;
        }
    }

    return NULL;
}
// return the address of the first element if the predicate returns true
// else NULL
void *DynArr_FindWithPredicate(DynArr this, bool(pred)(void *)) {
    for (size_t ix = 0; ix < this.size; ++ix) {
        void *current = this.arr_ptr + (ix * this.size_of_elem); 
        if (pred(current)) {
            return current; 
        }
    }

    return NULL;
}

// Deallocates half the memory if the array
// is occupied by less than a third
    // FIXME: causes craches
void DynArr_downsize(DynArr *this) {
    if ( this->capacity > (this->size * 3) )
        return;

    this->arr_ptr = realloc(this->arr_ptr, this->capacity / 2);
    assert(this->arr_ptr != NULL);

    this->capacity /= 2;
}

void DynArr_FilterOut(DynArr *this, void *elem_cmp) {
    size_t ix = 0;

    void * current = this->arr_ptr;
    while (ix < this->size) {
        if (memcmp(current, elem_cmp, this->size_of_elem) != 0) {
            ix += 1;
            current += this->size_of_elem;
            continue;
        }

        memmove(
            current, 
            current + this->size_of_elem,
            this->size_of_elem * (this->size - ix)
        );

        this->size -= 1;
    }

    // DynArr_downsize(this);
}