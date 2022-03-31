#include "minsk/analysis/binding/node/expression/assignment.h"
#include "minsk/analysis/binding/node.h"
#include <stdlib.h>

bound_expression_t* bound_assignment_expression_new(
    variable_symbol_t variable, bound_expression_t* expression) {
  bound_assignment_expression_t* assignment =
      malloc(sizeof(bound_assignment_expression_t));
  assignment->base.base.kind = bound_node_kind_assignment_expression;
  assignment->variable = variable;
  assignment->expression = expression;
  return (bound_expression_t*)assignment;
}

object_kind_t bound_assignment_expression_type(
    const bound_assignment_expression_t* expression) {
  return expression->variable.type;
}

void bound_assignment_expression_free(
    bound_assignment_expression_t* expression) {
  variable_symbol_free(&expression->variable);
  bound_node_free((bound_node_t*)expression->expression);
  free(expression);
}
