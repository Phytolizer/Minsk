#include "minsk/analysis/binding/node/expression.h"
#include "minsk/analysis/binding/node/expression/binary.h"
#include "minsk/analysis/binding/node/expression/literal.h"
#include "minsk/analysis/binding/node/expression/unary.h"
#include <assert.h>

object_kind_t bound_expression_type(const bound_expression_t *expression) {
  switch (expression->base.kind) {
  case bound_node_kind_binary_expression:
    return bound_binary_expression_type(
        (const bound_binary_expression_t *)expression);
  case bound_node_kind_literal_expression:
    return bound_literal_expression_type(
        (const bound_literal_expression_t *)expression);
  case bound_node_kind_unary_expression:
    return bound_unary_expression_type(
        (const bound_unary_expression_t *)expression);
  }
  assert(false && "corrupt bound expression");
}
