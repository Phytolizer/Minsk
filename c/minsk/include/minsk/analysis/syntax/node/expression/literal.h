#pragma once

#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/token.h"
#include "minsk/runtime/object.h"

typedef struct {
  expression_syntax_t base;
  syntax_token_t literal_token;
  object_t *value;
} literal_expression_syntax_t;

expression_syntax_t *literal_expression_syntax_new(syntax_token_t literal_token,
                                                   object_t *value);
void literal_expression_syntax_free(literal_expression_syntax_t *syntax);
syntax_node_children_t
literal_expression_syntax_children(const literal_expression_syntax_t *syntax);
