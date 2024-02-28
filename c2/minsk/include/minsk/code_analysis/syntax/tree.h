#pragma once

#include <arena.h>
#include <minsk-string/string.h>

#include "minsk/code_analysis/diagnostic_bag.h"
#include "minsk/code_analysis/syntax/ast/node.h"
#include "minsk/code_analysis/syntax/token.h"
#include "minsk/code_analysis/text/source_text.h"
#include "minsk/data_structures/buf.h"

typedef struct
{
  minsk_text_source_text_t text;
  minsk_diagnostic_bag_t diagnostics;
  minsk_syntax_node_t root;
  minsk_syntax_token_t end_of_file_token;
} minsk_syntax_tree_t;

extern minsk_syntax_tree_t
minsk_syntax_tree_parse(Arena * arena, string_t text);

typedef BUF_T(minsk_syntax_token_t) minsk_syntax_token_buf_t;
extern minsk_syntax_token_buf_t
minsk_syntax_tree_parse_tokens(Arena * arena, string_t text);
