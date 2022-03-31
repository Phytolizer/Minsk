#pragma once

#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/node/unit.h"
#include "minsk/analysis/syntax/token.h"
#include "minsk/analysis/syntax/tokens.h"
#include "minsk/analysis/text/source.h"

typedef struct {
  compilation_unit_syntax_t root;
  diagnostic_bag_t diagnostics;
  source_text_t source_text;
} syntax_tree_t;

void syntax_tree_free(syntax_tree_t* tree);
syntax_tree_t syntax_tree_parse(const char* text);
syntax_token_vector_t syntax_tree_parse_tokens(const char* text);
