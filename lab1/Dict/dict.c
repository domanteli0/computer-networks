#include "dict.h"

Dict Dict_WithCapacity(size_t capacity,
                       size_t key_size,
                       size_t value_size)
{
    DynArr arr = DynArr_WithCapacity(
        capacity,
        key_size + value_size);

    Dict ret = {
        .dyn_arr = arr,
        .key_size = key_size,
        .value_size = value_size,
        .reserved = calloc(key_size + value_size, 1),
    };

    assert(ret.reserved != NULL);

    return ret;
}

void Dict_Insert(Dict *this, void *key, void *value)
{
    assert(this->dyn_arr.arr_ptr != NULL);
    assert(key != NULL);
    assert(value != NULL);

    memcpy(this->reserved, key, this->key_size);
    memcpy(this->reserved + (this->key_size), value, this->value_size);
    DynArr_Push(&(this->dyn_arr), this->reserved);
}

void *Dict_Get(Dict this, void *key)
{
    assert(this.dyn_arr.arr_ptr != NULL);
    assert(key != NULL);

    void *kvp_ptr = this.dyn_arr.arr_ptr;
    for (size_t ix = 0; ix < (this.dyn_arr).size; ++ix)
    {
        kvp_ptr += this.dyn_arr.size_of_elem;
        if (memcmp(kvp_ptr, key, this.key_size) == 0)
        {
            return kvp_ptr;
        }
    }

    return NULL;
}

void Dict_Free(Dict this)
{
    free(this.reserved);
    DynArr_Free(this.dyn_arr);
}