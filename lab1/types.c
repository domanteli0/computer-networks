#include <stdlib.h>
#include "dict.h"

#define Float1 float

typedef struct Float2 {
    Float1 x;
    Float1 y;
} Float2; 

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

typedef struct Header {
    uint16_t type_id;
} Header;

typedef struct RGBA {
    uint8_t r;
    uint8_t g;
    uint8_t b;
    uint8_t a;
} RGBA;

#define ITEM_DOT_TYPE_ID 0x0000

typedef struct IDot {
    uint16_t type_id;
    Float1 x;
    Float1 y;
} IDot;

char *IDot_ToString(IDot this) {
    char *str = calloc(1024, 1);
    sprintf(str, "Type: 0x%x, x: %f, y: %f", this.type_id, this.x, this.y);
    return str;
}

IDot IDot_FromFloat2(Float2 f2) {
    IDot ret = {
        .type_id = ITEM_DOT_TYPE_ID,
        .x = f2.x,
        .y = f2.y,
    };

    return ret;
}

IDot IDot_new(Float1 x, Float1 y) {
    IDot ret = {
        .type_id = ITEM_DOT_TYPE_ID,
        .x = x,
        .y = y,
    };

    return ret;
}