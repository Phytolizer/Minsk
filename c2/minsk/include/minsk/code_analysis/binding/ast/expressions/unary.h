#pragma once

#include "./unary_operator.h"

typedef struct
{
  minsk_bound_expression_unary_operator_kind_t op_kind;
  struct minsk_bound_node * operand;
} minsk_bound_expression_unary_t;

#define MINSK_BOUND_EXPRESSION_UNARY(...)   \
  MINSK_BOUND_EXPRESSION(                   \
    MINSK_BOUND_NODE_TYPE_UNARY_EXPRESSION, \
    .unary = {__VA_ARGS__}                  \
  )
