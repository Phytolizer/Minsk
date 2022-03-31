#include "minsk/analysis/binding/scope.h"
#include <stdlib.h>

void bound_scope_init(bound_scope_t* scope, bound_scope_t* parent) {
  bound_scope_map_init(&scope->variables);
  scope->parent = parent;
}

bool bound_scope_try_declare(
    bound_scope_t* scope, sds name, variable_symbol_t value) {
  if (bound_scope_map_lookup(&scope->variables, name) != NULL) {
    return false;
  }

  bound_scope_map_insert(&scope->variables, name, value);
  return true;
}

variable_symbol_t* bound_scope_try_lookup(
    const bound_scope_t* scope, const sds name) {
  variable_symbol_t* symbol = bound_scope_map_lookup(&scope->variables, name);
  if (symbol != NULL) {
    return symbol;
  }

  if (scope->parent != NULL) {
    return bound_scope_try_lookup(scope->parent, name);
  }

  return NULL;
}

variable_symbol_vector_t bound_scope_get_declared_variables(
    const bound_scope_t* scope) {
  variable_symbol_vector_t result;
  variable_symbol_vector_init(&result);

  for (size_t i = 0; i < scope->variables.capacity; i++) {
    if (scope->variables.data[i].name != NULL) {
      variable_symbol_vector_push(
          &result, variable_symbol_copy(&scope->variables.data[i].value));
    }
  }

  return result;
}

void bound_scope_free(bound_scope_t* scope) {
  if (scope->parent != NULL) {
    bound_scope_free(scope->parent);
  }
  bound_scope_map_free(&scope->variables);
  free(scope);
}
