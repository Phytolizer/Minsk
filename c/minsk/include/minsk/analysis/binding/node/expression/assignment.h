#pragma once

#include "minsk/analysis/binding/node/expression.h"
#include "minsk/analysis/symbol.h"

typedef struct {
  bound_expression_t base;
  variable_symbol_t variable;
  bound_expression_t* expression;
} bound_assignment_expression_t;

bound_expression_t* bound_assignment_expression_new(
    variable_symbol_t variable,
    bound_expression_t* expression
);
object_kind_t
bound_assignment_expression_type(const bound_assignment_expression_t* expression
);
void bound_assignment_expression_free(bound_assignment_expression_t* expression
);
