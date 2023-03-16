#include <stdlib.h>
#include "../Dict/dict.h"
#include "idot.c"

// Creates a new Float2 on the stack
Float2 Float2_New(float f1, float f2) {
    Float2 ret = {
        .x = f1,
        .y = f2,        
    };

    return ret;
}

// Creates a new Float2 on the heap
// the caller must free the float2  
Float2 *Float2_NewPtr(float f1, float f2) {
    Float2 *ptr = calloc(1, sizeof(Float2));

    memcpy(&ptr->x, &f1, sizeof(float));
    memcpy(&ptr->y, &f2, sizeof(float));
    
    return ptr;
}

// Stringifies a `Float2`.
// The caller is responsible for freeing the string with `free()` 
char *Float2_ToString(Float2 this) {
    const size_t MAX_STR_LEN = 128;
    char *str = calloc(MAX_STR_LEN, sizeof(char));

    sprintf(str, "(%f, %f)", this.x, this.y);
    return str;
}

// typedef struct Canvas {
//     Float2 pos; 
//     Float1 zoom_in_level;

//     DynArr points; // an array of points
// } Canvas;

typedef struct RGBA {
    uint8_t r;
    uint8_t g;
    uint8_t b;
    uint8_t a;
} RGBA;

// FIXME: Ideally, i would use different types
// to not include client side rendering code
// in the same struct that is used in the server side code
// but im a bit too lazy to do this right now

// data field is ready as is to be sent over the network
typedef struct Item Item;
struct Item {
    void *data;
    size_t size;

    void (*Free)(Item);

    // TODO:
    void (*Render)();
};

typedef struct {
    uint32_t type_size; // amount of data to read

    // The returned type copies the data
    // Thus the caller is responsible for calling Item->Free
    // after use
    //
    // It is assumed that data does not contain the header
    Item (*VoidToTyped)(void *data); // converts received bytes to a specific type
} BytesConverter;

// typedef struct IDog

// Maps type_id's to BytesConverter structs 
// The caller needs to call Dict_Free, 
// after the use of this dictionary
Dict TypeIdToBytesConverted() {
    Dict ret = Dict_WithCapacity(1, sizeof(uint32_t), sizeof(BytesConverter));

    return ret;
} 