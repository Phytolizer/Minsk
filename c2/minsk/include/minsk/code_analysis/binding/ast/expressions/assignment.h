#pragma once

#include <minsk-string/string.h>

#include "minsk/code_analysis/variable_symbol.h"

typedef struct
{
  minsk_variable_symbol_t variable;
  struct minsk_bound_node * expression;
} minsk_bound_expression_assignment_t;

#define MINSK_BOUND_EXPRESSION_ASSIGNMENT(...)   \
  MINSK_BOUND_EXPRESSION(                        \
    MINSK_BOUND_NODE_TYPE_ASSIGNMENT_EXPRESSION, \
    .assignment = {__VA_ARGS__}                  \
  )
