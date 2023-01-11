#pragma once

#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/token.h"

typedef struct {
  expression_syntax_t base;
  syntax_token_t open_parenthesis_token;
  expression_syntax_t* expression;
  syntax_token_t close_parenthesis_token;
} parenthesized_expression_syntax_t;

expression_syntax_t* parenthesized_expression_syntax_new(
    syntax_token_t open_parenthesis_token,
    expression_syntax_t* expression,
    syntax_token_t close_parenthesis_token
);
void parenthesized_expression_syntax_free(
    parenthesized_expression_syntax_t* syntax
);
syntax_node_children_t parenthesized_expression_syntax_children(
    parenthesized_expression_syntax_t* syntax
);
