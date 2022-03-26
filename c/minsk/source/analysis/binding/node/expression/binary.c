#include "minsk/analysis/binding/node/expression/binary.h"
#include "minsk/analysis/binding/node.h"
#include "minsk/analysis/binding/node/expression.h"
#include "minsk/analysis/binding/node/expression/binary/operator.h"
#include "minsk/runtime/object.h"
#include <stdlib.h>
bound_expression_t *
bound_binary_expression_new(bound_expression_t *left,
                            const bound_binary_operator_t *op,
                            bound_expression_t *right) {
  bound_binary_expression_t *expression =
      malloc(sizeof(bound_binary_expression_t));
  expression->base.base.kind = bound_node_kind_binary_expression;
  expression->left = left;
  expression->op = op;
  expression->right = right;
  return (bound_expression_t *)expression;
}

object_kind_t
bound_binary_expression_type(const bound_binary_expression_t *expression) {
  if (expression->op == NULL) {
    return object_kind_null;
  }
  return expression->op->result_type;
}

void bound_binary_expression_free(bound_binary_expression_t *expression) {
  bound_node_free((bound_node_t *)expression->left);
  bound_node_free((bound_node_t *)expression->right);
  free(expression);
}
