#pragma once

#include "minsk/analysis/syntax/node.h"
#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/token.h"

typedef struct {
  syntax_node_t base;
  expression_syntax_t* root;
  syntax_token_t end_of_file_token;
} compilation_unit_syntax_t;

void compilation_unit_syntax_init(compilation_unit_syntax_t* unit,
    expression_syntax_t* root, syntax_token_t end_of_file_token);
void compilation_unit_syntax_free(compilation_unit_syntax_t* unit);
