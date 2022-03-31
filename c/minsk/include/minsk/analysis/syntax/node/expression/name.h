#pragma once

#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/token.h"

typedef struct {
  expression_syntax_t base;
  syntax_token_t identifier_token;
} name_expression_syntax_t;

expression_syntax_t* name_expression_syntax_new(
    syntax_token_t identifier_token);
void name_expression_syntax_free(name_expression_syntax_t* syntax);
syntax_node_children_t name_expression_syntax_children(
    const name_expression_syntax_t* syntax);
