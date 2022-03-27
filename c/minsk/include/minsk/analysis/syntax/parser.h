#pragma once

#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/tree.h"
#include "minsk/analysis/text/source.h"
#include "peek_buffer.h"

typedef struct {
  peek_buffer_t peek_buffer;
  diagnostic_bag_t diagnostics;
} parser_t;

void parser_init(parser_t *parser, source_text_t text);
syntax_tree_t parser_parse(parser_t *parser);
void parser_free(parser_t *parser);
