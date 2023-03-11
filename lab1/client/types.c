#include <stdlib.h>
#include <SDL2/SDL.h>

#define Float1 float

// Used to represent points and vectors
typedef SDL_FPoint Float2;

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
