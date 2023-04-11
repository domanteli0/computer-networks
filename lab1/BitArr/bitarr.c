#include "bitarr.h"

void checkFromZXToU8(size_t size) {
    size_t max_u8 = (size_t) ((uint8_t) -1);
    assert(size <= max_u8);
}

BitArr BitArr_New() {
    BitArr ret = { .data = {0, 0, 0, 0,} };
    return ret;
}

void BitArr_Flip(BitArr *this, uint8_t pos) {
    uint8_t index = pos / 64;
    pos %= 64;

    this->data[index] ^= (1 << pos);
}

void BitArr_FlipU(BitArr *this, size_t pos) {
    checkFromZXToU8(pos);
    
    BitArr_Flip(this, (uint8_t) pos);
}

bool BitArr_Get(BitArr this, uint8_t pos) {
    return((this.data[pos/64] & (1 << (pos%64) )) != 0 );     
}

bool BitArr_GetU(BitArr this, size_t pos) {
    checkFromZXToU8(pos);
    return BitArr_Get(this, (uint8_t) pos);
}

void BitArr_Clear(BitArr *this, uint8_t pos) {
    this->data[pos/64] &= ~(1 << (pos%64));
}

void BitArr_Set(BitArr *this, uint8_t pos) {
    this->data[pos/64] |= 1 << (pos%64);
}
