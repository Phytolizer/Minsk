#pragma once

#include <arena.h>
#include <minsk-string/string.h>
#include <stddef.h>

#include "minsk/code_analysis/diagnostic_buf.h"
#include "minsk/data_structures/buf.h"

#include "./token.h"
#include "./tree.h"

typedef BUF_T(minsk_syntax_token_t, parser_token)
  minsk_syntax_parser_token_buf_t;

typedef struct
{
  Arena * _arena;
  minsk_syntax_parser_token_buf_t _tokens;
  size_t _position;
  minsk_diagnostic_buf_t diagnostics;
} minsk_syntax_parser_t;

extern minsk_syntax_parser_t
minsk_syntax_parser_new(Arena * arena, string_t text);

extern minsk_syntax_tree_t
minsk_syntax_parser_parse(minsk_syntax_parser_t * parser);
