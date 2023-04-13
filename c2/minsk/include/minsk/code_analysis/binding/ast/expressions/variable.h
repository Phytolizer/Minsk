#pragma once

#include <minsk-string/string.h>

#include "minsk/code_analysis/variable_symbol.h"
#include "minsk/runtime/object.h"

typedef struct
{
  minsk_variable_symbol_t variable;
} minsk_bound_expression_variable_t;

#define MINSK_BOUND_EXPRESSION_VARIABLE(...)  \
 MINSK_BOUND_EXPRESSION(                      \
   MINSK_BOUND_NODE_TYPE_VARIABLE_EXPRESSION, \
   .variable = {__VA_ARGS__}                  \
 )
