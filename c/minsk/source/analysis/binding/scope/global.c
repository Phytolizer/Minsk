#include "minsk/analysis/binding/scope/global.h"
#include "minsk/analysis/binding/node.h"

void bound_global_scope_init(bound_global_scope_t *scope,
                             bound_global_scope_t *previous,
                             diagnostic_bag_t diagnostics,
                             variable_symbol_vector_t variables,
                             bound_expression_t *expression) {
  scope->previous = previous;
  scope->diagnostics = diagnostics;
  scope->variables = variables;
  scope->expression = expression;
}

void bound_global_scope_free(bound_global_scope_t *scope) {
  if (scope->previous != NULL) {
    bound_global_scope_free(scope->previous);
  }
  bound_node_free((bound_node_t *)scope->expression);
  variable_symbol_vector_free(&scope->variables);
  diagnostic_bag_free(&scope->diagnostics);
}
