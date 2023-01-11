#pragma once

#include "minsk/analysis/binding/node/expression.h"
#include "minsk/analysis/binding/scope.h"
#include "minsk/analysis/binding/scope/global.h"
#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/node/unit.h"
#include "minsk/analysis/syntax/tree.h"
#include "minsk/analysis/variables.h"

typedef struct {
  diagnostic_bag_t diagnostics;
  bound_scope_t* scope;
} binder_t;

void binder_init(binder_t* binder, bound_scope_t* parent);
bound_global_scope_t binder_bind_global_scope(
    bound_global_scope_t* previous,
    const compilation_unit_syntax_t* syntax
);
bound_expression_t*
binder_bind_expression(binder_t* binder, const expression_syntax_t* expression);
void binder_free(binder_t* binder);
