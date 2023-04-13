#pragma once

#include <minsk-string/string.h>

#include "minsk/runtime/object.h"

typedef struct
{
  string_t name;
  minsk_object_type_t type;
} minsk_bound_expression_variable_t;

#define MINSK_BOUND_EXPRESSION_VARIABLE(...)  \
 MINSK_BOUND_EXPRESSION(                      \
   MINSK_BOUND_NODE_TYPE_VARIABLE_EXPRESSION, \
   .variable = {__VA_ARGS__}                  \
 )
