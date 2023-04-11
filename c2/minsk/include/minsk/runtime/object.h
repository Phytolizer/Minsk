#pragma once

#include <bstrlib.h>
#include <stdint.h>

typedef enum {
#define X(x) MINSK_OBJECT_TYPE_##x,
#include "./private/object_types.xmacro"
#undef X
} minsk_object_type_t;

typedef struct {
  minsk_object_type_t type;
  union {
    int64_t integer;
  };
} minsk_object_t;

#define MINSK_OBJECT_INTEGER(i) \
  ((minsk_object_t){.type = MINSK_OBJECT_TYPE_INTEGER, .integer = (i)})

extern bstring minsk_object_type_display_name(minsk_object_type_t type);
