#pragma once

#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/token.h"

typedef struct {
  expression_syntax_t base;
  expression_syntax_t *left;
  syntax_token_t operator_token;
  expression_syntax_t *right;
} binary_expression_syntax_t;

expression_syntax_t *binary_expression_syntax_new(expression_syntax_t *left,
                                                  syntax_token_t operator_token,
                                                  expression_syntax_t *right);
void binary_expression_syntax_free(binary_expression_syntax_t *syntax);
syntax_node_children_t
binary_expression_syntax_children(const binary_expression_syntax_t *syntax);
