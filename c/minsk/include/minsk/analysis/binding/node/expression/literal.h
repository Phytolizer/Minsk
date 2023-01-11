#pragma once

#include "minsk/analysis/binding/node/expression.h"
#include "minsk/runtime/object.h"

typedef struct {
  bound_expression_t base;
  object_t* value;
} bound_literal_expression_t;

bound_expression_t* bound_literal_expression_new(object_t* value);
object_kind_t
bound_literal_expression_type(const bound_literal_expression_t* expression);
void bound_literal_expression_free(bound_literal_expression_t* expression);
