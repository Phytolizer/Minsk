#include "minsk/analysis/binding/node.h"
#include "minsk/analysis/binding/kind.h"
#include "minsk/analysis/binding/node/expression/assignment.h"
#include "minsk/analysis/binding/node/expression/binary.h"
#include "minsk/analysis/binding/node/expression/literal.h"
#include "minsk/analysis/binding/node/expression/unary.h"
#include "minsk/analysis/binding/node/expression/variable.h"
#include <assert.h>

void bound_node_free(bound_node_t* node) {
  switch (node->kind) {
  case bound_node_kind_assignment_expression:
    bound_assignment_expression_free((bound_assignment_expression_t*)node);
    return;
  case bound_node_kind_binary_expression:
    bound_binary_expression_free((bound_binary_expression_t*)node);
    return;
  case bound_node_kind_literal_expression:
    bound_literal_expression_free((bound_literal_expression_t*)node);
    return;
  case bound_node_kind_unary_expression:
    bound_unary_expression_free((bound_unary_expression_t*)node);
    return;
  case bound_node_kind_variable_expression:
    bound_variable_expression_free((bound_variable_expression_t*)node);
    return;
  }
  assert(false && "corrupt or unhandled node kind");
}
