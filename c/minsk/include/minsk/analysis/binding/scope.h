#pragma once

#include "minsk/analysis/binding/scope/map.h"
#include "minsk/analysis/symbol.h"
#include "minsk/analysis/symbols.h"
#include <stdbool.h>

typedef struct minsk_bound_scope {
  bound_scope_map_t variables;
  struct minsk_bound_scope* parent;
} bound_scope_t;

void bound_scope_init(bound_scope_t* scope, bound_scope_t* parent);
bool bound_scope_try_declare(
    bound_scope_t* scope, sds name, variable_symbol_t value);
variable_symbol_t* bound_scope_try_lookup(
    const bound_scope_t* scope, const sds name);
variable_symbol_vector_t bound_scope_get_declared_variables(
    const bound_scope_t* scope);
void bound_scope_free(bound_scope_t* scope);
