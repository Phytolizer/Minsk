#pragma once

#include <bstrlib.h>

#include "./kind.h"
#include "minsk/runtime/object.h"

typedef struct {
  minsk_syntax_kind_t kind;
  int position;
  bstring text;
  minsk_object_t value;
} minsk_syntax_token_t;
