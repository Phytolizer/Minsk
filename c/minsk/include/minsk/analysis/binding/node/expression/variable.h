#pragma once

#include "minsk/analysis/binding/node/expression.h"
#include "minsk/analysis/symbol.h"
#include "sds.h"

typedef struct {
  bound_expression_t base;
  variable_symbol_t variable;
} bound_variable_expression_t;

bound_expression_t* bound_variable_expression_new(variable_symbol_t variable);
object_kind_t
bound_variable_expression_type(const bound_variable_expression_t* expression);
void bound_variable_expression_free(bound_variable_expression_t* expression);
