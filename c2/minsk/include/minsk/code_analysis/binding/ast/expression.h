#pragma once

#include "minsk/runtime/object.h"

#include "./expressions/binary.h"
#include "./expressions/literal.h"
#include "./expressions/unary.h"
#include "./node_type.h"

typedef struct
{
  minsk_bound_node_type_t type;

  union
  {
    minsk_bound_expression_binary_t binary;
    minsk_bound_expression_literal_t literal;
    minsk_bound_expression_unary_t unary;
  };
} minsk_bound_expression_t;

#define MINSK_BOUND_EXPRESSION(ty, ...)       \
 ((minsk_bound_node_t){                       \
   .type = (ty),                              \
   .expression = {.type = (ty), __VA_ARGS__}, \
 })

extern minsk_object_type_t
minsk_bound_expression_get_resolved_type(minsk_bound_expression_t expression);
