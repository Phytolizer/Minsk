#include "minsk/runtime/object.h"

#include <stdbool.h>

static bstring prettify(const_bstring str) {
  bstring result = blk2bstr("", 0);
  ballocmin(result, blength(str));

  bool next_upper = true;
  for (int i = 0; i < blength(str); i++) {
    char c = bchar(str, i);
    if (c == '_') {
      next_upper = true;
    } else if (next_upper) {
      next_upper = false;
      bconchar(result, c);
    } else {
      bconchar(result, tolower(c));
    }
  }

  return result;
}

extern bstring minsk_object_type_display_name(minsk_object_type_t type) {
  switch (type) {
#define X(x)                          \
  case MINSK_OBJECT_TYPE_##x: {       \
    struct tagbstring t;              \
    blk2tbstr(t, #x, sizeof(#x) - 1); \
    return prettify(&t);              \
  }
#include "minsk/runtime/private/object_types.xmacro"
#undef X
  }
}
