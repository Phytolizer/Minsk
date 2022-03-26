#pragma once

#include "minsk/analysis/binding/node/expression.h"
#include "minsk/analysis/binding/node/expression/unary/operator.h"

typedef struct {
  bound_expression_t base;
  const bound_unary_operator_t *op;
  bound_expression_t *operand;
} bound_unary_expression_t;

bound_expression_t *bound_unary_expression_new(const bound_unary_operator_t *op,
                                               bound_expression_t *operand);
object_kind_t
bound_unary_expression_type(const bound_unary_expression_t *expression);
void bound_unary_expression_free(bound_unary_expression_t *expression);
