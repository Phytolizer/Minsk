#pragma once

#include "./ast/node.h"
#include "minsk/code_analysis/diagnostic_buf.h"

typedef struct
{
  minsk_diagnostic_buf_t diagnostics;
  minsk_syntax_node_t root;
  minsk_syntax_token_t end_of_file_token;
} minsk_syntax_tree_t;
