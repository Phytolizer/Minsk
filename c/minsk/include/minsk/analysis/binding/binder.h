#pragma once

#include "minsk/analysis/binding/node/expression.h"
#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/tree.h"

typedef struct {
  diagnostic_bag_t diagnostics;
} binder_t;

void binder_init(binder_t *binder);
bound_expression_t *
binder_bind_expression(binder_t *binder, const expression_syntax_t *expression);
void binder_free(binder_t *binder);
