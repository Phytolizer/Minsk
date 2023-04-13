#pragma once

#include <arena.h>
#include <minsk-string/string.h>
#include <stdint.h>
#include <stdio.h>

#include "minsk/meta/concat.h"

#define MINSK_PREFIX_OBJECT_TYPE MINSK_OBJECT_TYPE_

typedef enum
{

#define X(x) MINSK_CONCAT(MINSK_PREFIX_OBJECT_TYPE, x),
#include "./private/object_types.xmacro"
#undef X
} minsk_object_type_t;

typedef struct
{
  minsk_object_type_t type;

  union
  {
    int64_t integer;
    bool boolean;
  };
} minsk_object_t;

#define MINSK_OBJECT_INTEGER(i) \
 ((minsk_object_t){.type = MINSK_OBJECT_TYPE_INTEGER, .integer = (i)})
#define MINSK_OBJECT_BOOLEAN(b) \
 ((minsk_object_t){.type = MINSK_OBJECT_TYPE_BOOLEAN, .boolean = (b)})
#define MINSK_OBJECT_NIL ((minsk_object_t){.type = MINSK_OBJECT_TYPE_NIL})

extern string_t
minsk_object_type_display_name(Arena * arena, minsk_object_type_t type);
extern int
minsk_object_show(minsk_object_t object, FILE * stream);
