#pragma once

#include <arena.h>
#include <stddef.h>

#include "minsk/code_analysis/diagnostic_bag.h"
#include "minsk/code_analysis/syntax/token.h"
#include "minsk/code_analysis/syntax/tree.h"
#include "minsk/code_analysis/text/source_text.h"
#include "minsk/data_structures/buf.h"

typedef BUF_T(minsk_syntax_token_t) minsk_syntax_parser_token_buf_t;

typedef struct
{
  Arena * _arena;
  minsk_text_source_text_t _text;
  minsk_syntax_parser_token_buf_t _tokens;
  size_t _position;
  minsk_diagnostic_bag_t diagnostics;
} minsk_syntax_parser_t;

extern minsk_syntax_parser_t
minsk_syntax_parser_new(Arena * arena, minsk_text_source_text_t text);

extern minsk_syntax_tree_t
minsk_syntax_parser_parse(minsk_syntax_parser_t * parser);
