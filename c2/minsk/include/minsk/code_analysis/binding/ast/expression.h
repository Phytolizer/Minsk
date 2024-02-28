#pragma once

#include "minsk/code_analysis/binding/ast/expressions/assignment.h"
#include "minsk/code_analysis/binding/ast/expressions/binary.h"
#include "minsk/code_analysis/binding/ast/expressions/literal.h"
#include "minsk/code_analysis/binding/ast/expressions/unary.h"
#include "minsk/code_analysis/binding/ast/expressions/variable.h"
#include "minsk/code_analysis/binding/ast/node_type.h"
#include "minsk/runtime/object.h"

typedef struct
{
  minsk_bound_node_type_t type;

  union
  {
    minsk_bound_expression_assignment_t assignment;
    minsk_bound_expression_binary_t binary;
    minsk_bound_expression_literal_t literal;
    minsk_bound_expression_unary_t unary;
    minsk_bound_expression_variable_t variable;
  };
} minsk_bound_expression_t;

#define MINSK_BOUND_EXPRESSION(ty, ...) \
  ((minsk_bound_node_t){ \
    .type = (ty), \
    .expression = {.type = (ty), __VA_ARGS__}, \
  })

extern minsk_object_type_t
minsk_bound_expression_get_resolved_type(minsk_bound_expression_t expression);
