#pragma once

#include "minsk/code_analysis/syntax/ast/expressions/assignment.h"  // IWYU pragma: export
#include "minsk/code_analysis/syntax/ast/expressions/binary.h"  // IWYU pragma: export
#include "minsk/code_analysis/syntax/ast/expressions/literal.h"  // IWYU pragma: export
#include "minsk/code_analysis/syntax/ast/expressions/name.h"  // IWYU pragma: export
#include "minsk/code_analysis/syntax/ast/expressions/parenthesized.h"  // IWYU pragma: export
#include "minsk/code_analysis/syntax/ast/expressions/unary.h"  // IWYU pragma: export
#include "minsk/code_analysis/syntax/ast/node_type.h"

typedef struct
{
  minsk_syntax_node_type_t type;

  union
  {
    minsk_syntax_expression_assignment_t assignment;
    minsk_syntax_expression_binary_t binary;
    minsk_syntax_expression_literal_t literal;
    minsk_syntax_expression_name_t name;
    minsk_syntax_expression_parenthesized_t parenthesized;
    minsk_syntax_expression_unary_t unary;
  };
} minsk_syntax_expression_t;

#define MINSK_SYNTAX_EXPRESSION(ty, ...) \
  ((minsk_syntax_node_t){ \
    .type = (ty), \
    .expression = {.type = (ty), __VA_ARGS__}, \
  })
