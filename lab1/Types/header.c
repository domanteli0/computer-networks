#include "header.h"

void DrawableHeader_Printf(DrawableHeader this) {
    printf("[ Type: 0x%" PRIX16 " | Size: %" PRIu16 " ]", this.type_id, this.size);
}
