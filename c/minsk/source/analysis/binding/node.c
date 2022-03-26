#include "minsk/analysis/binding/node.h"
#include "minsk/analysis/binding/kind.h"
#include "minsk/analysis/binding/node/expression/binary.h"
#include "minsk/analysis/binding/node/expression/literal.h"
#include "minsk/analysis/binding/node/expression/unary.h"

void bound_node_free(bound_node_t *node) {
  switch (node->kind) {
  case bound_node_kind_binary_expression:
    bound_binary_expression_free((bound_binary_expression_t *)node);
    break;
  case bound_node_kind_literal_expression:
    bound_literal_expression_free((bound_literal_expression_t *)node);
    break;
  case bound_node_kind_unary_expression:
    bound_unary_expression_free((bound_unary_expression_t *)node);
    break;
  }
}
