#pragma once

#include "./expression.h"
#include "./node_type.h"

typedef struct minsk_bound_node
{
  minsk_bound_node_type_t type;
  union
  {
    minsk_bound_expression_t expression;
  };
} minsk_bound_node_t;

extern minsk_bound_node_t *
minsk_bound_node_promote(Arena * arena, minsk_bound_node_t node);
