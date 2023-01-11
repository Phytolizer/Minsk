#pragma once

#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/token.h"

typedef struct {
  expression_syntax_t base;
  syntax_token_t identifier_token;
  syntax_token_t equals_token;
  expression_syntax_t* expression;
} assignment_expression_syntax_t;

expression_syntax_t* assignment_expression_syntax_new(
    syntax_token_t identifier_token,
    syntax_token_t equals_token,
    expression_syntax_t* expression
);
void assignment_expression_syntax_free(assignment_expression_syntax_t* syntax);
syntax_node_children_t assignment_expression_syntax_children(
    const assignment_expression_syntax_t* syntax
);
