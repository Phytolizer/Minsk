#pragma once

#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/token.h"

typedef struct {
  expression_syntax_t base;
  syntax_token_t operator_token;
  expression_syntax_t* operand;
} unary_expression_syntax_t;

expression_syntax_t* unary_expression_syntax_new(
    syntax_token_t operator_token, expression_syntax_t* operand);
void unary_expression_syntax_free(unary_expression_syntax_t* syntax);
syntax_node_children_t unary_expression_syntax_children(
    unary_expression_syntax_t* syntax);
