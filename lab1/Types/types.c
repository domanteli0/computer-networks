// #include "../Dict/dict.h"
// #include "idot.c"

// typedef struct Canvas {
//     Float2 pos; 
//     Float1 zoom_in_level;

//     DynArr points; // an array of points
// } Canvas;

// typedef struct RGBA {
//     uint8_t r;
//     uint8_t g;
//     uint8_t b;
//     uint8_t a;
// } RGBA;

// FIXME: Ideally, i would use different types
// to not include client side rendering code
// in the same struct that is used in the server side code
// but im a bit too lazy to do this right now

// // data field is ready as is to be sent over the network
// typedef struct Item Item;
// struct Item {
//     void *data;
//     size_t size;

//     void (*Free)(Item);

//     // TODO:
//     void (*Render)();
// };

// typedef struct {
//     uint32_t type_size; // amount of data to read

//     // The returned type copies the data
//     // Thus the caller is responsible for calling Item->Free
//     // after use
//     //
//     // It is assumed that data does not contain the header
//     Item (*VoidToTyped)(void *data); // converts received bytes to a specific type
// } BytesConverter;

// typedef struct IDog

// // Maps type_id's to BytesConverter structs 
// // The caller needs to call Dict_Free, 
// // after the use of this dictionary
// Dict TypeIdToBytesConverted() {
//     Dict ret = Dict_WithCapacity(1, sizeof(uint32_t), sizeof(BytesConverter));

//     return ret;
// }
