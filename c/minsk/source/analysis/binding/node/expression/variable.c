#include "minsk/analysis/binding/node/expression/variable.h"
#include <stdlib.h>

bound_expression_t* bound_variable_expression_new(variable_symbol_t variable) {
  bound_variable_expression_t* expression =
      malloc(sizeof(bound_variable_expression_t));
  expression->base.base.kind = bound_node_kind_variable_expression;
  expression->variable = variable;
  return (bound_expression_t*)expression;
}

object_kind_t
bound_variable_expression_type(const bound_variable_expression_t* expression) {
  return expression->variable.type;
}

void bound_variable_expression_free(bound_variable_expression_t* expression) {
  variable_symbol_free(&expression->variable);
  free(expression);
}
