#pragma once

#include <minsk-string/string.h>

#include "minsk/runtime/object.h"

typedef struct
{
  string_t name;
  minsk_object_type_t type;
} minsk_variable_symbol_t;
