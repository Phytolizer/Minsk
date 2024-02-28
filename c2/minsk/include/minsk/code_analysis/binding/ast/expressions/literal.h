#pragma once

#include "minsk/runtime/object.h"

typedef struct
{
  minsk_object_t value;
} minsk_bound_expression_literal_t;

#define MINSK_BOUND_EXPRESSION_LITERAL(...) \
  MINSK_BOUND_EXPRESSION( \
    MINSK_BOUND_NODE_TYPE_LITERAL_EXPRESSION, \
    .literal = {__VA_ARGS__} \
  )
