#pragma once

#include "minsk/analysis/binding/node.h"
#include "minsk/runtime/object.h"

typedef struct {
  bound_node_t base;
} bound_expression_t;

object_kind_t bound_expression_type(const bound_expression_t *expression);
