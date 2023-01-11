#pragma once

#include "minsk/analysis/binding/node/expression.h"
#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/symbols.h"

typedef struct minsk_bound_global_scope {
  struct minsk_bound_global_scope* previous;
  diagnostic_bag_t diagnostics;
  variable_symbol_vector_t variables;
  bound_expression_t* expression;
} bound_global_scope_t;

void bound_global_scope_init(
    bound_global_scope_t* scope,
    bound_global_scope_t* previous,
    diagnostic_bag_t diagnostics,
    variable_symbol_vector_t variables,
    bound_expression_t* expression
);
void bound_global_scope_free(bound_global_scope_t* scope);
