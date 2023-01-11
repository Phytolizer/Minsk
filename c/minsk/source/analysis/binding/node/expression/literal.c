#include "minsk/analysis/binding/node/expression/literal.h"
#include "minsk/analysis/binding/kind.h"
#include "minsk/runtime/object.h"
#include <stdlib.h>

bound_expression_t* bound_literal_expression_new(object_t* value) {
  bound_literal_expression_t* expression =
      malloc(sizeof(bound_literal_expression_t));
  expression->base.base.kind = bound_node_kind_literal_expression;
  expression->value = value;
  return (bound_expression_t*)expression;
}

object_kind_t
bound_literal_expression_type(const bound_literal_expression_t* expression) {
  if (expression->value == NULL) {
    return object_kind_null;
  }
  return expression->value->kind;
}

void bound_literal_expression_free(bound_literal_expression_t* expression) {
  object_free(expression->value);
  free(expression);
}
