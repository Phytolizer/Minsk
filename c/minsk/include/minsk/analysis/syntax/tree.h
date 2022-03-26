#pragma once

#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/token.h"

typedef struct {
  expression_syntax_t *root;
  syntax_token_t end_of_file_token;
  diagnostic_bag_t diagnostics;
} syntax_tree_t;

void syntax_tree_free(syntax_tree_t *tree);
syntax_tree_t syntax_tree_parse(const char *text);
