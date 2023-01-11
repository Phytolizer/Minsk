#include "minsk/analysis/binding/node/expression/unary.h"
#include "minsk/analysis/binding/kind.h"
#include "minsk/analysis/binding/node/expression.h"
#include "minsk/analysis/binding/node/expression/unary/operator.h"
#include "minsk/runtime/object.h"
#include <stdlib.h>

bound_expression_t* bound_unary_expression_new(
    const bound_unary_operator_t* op,
    bound_expression_t* operand
) {
  bound_unary_expression_t* expression =
      malloc(sizeof(bound_unary_expression_t));
  expression->base.base.kind = bound_node_kind_unary_expression;
  expression->op = op;
  expression->operand = operand;
  return (bound_expression_t*)expression;
}

object_kind_t
bound_unary_expression_type(const bound_unary_expression_t* expression) {
  if (expression->op == NULL) {
    return object_kind_null;
  }
  return expression->op->result_type;
}

void bound_unary_expression_free(bound_unary_expression_t* expression) {
  bound_node_free((bound_node_t*)expression->operand);
  free(expression);
}
