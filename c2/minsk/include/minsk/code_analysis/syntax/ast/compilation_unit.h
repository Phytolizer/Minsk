#pragma once

#include "minsk/code_analysis/syntax/token.h"

typedef struct
{
  struct minsk_syntax_node * expression;
  minsk_syntax_token_t end_of_file_token;
} minsk_syntax_compilation_unit_t;

#define MINSK_SYNTAX_COMPILATION_UNIT(...) \
  ((minsk_syntax_node_t){ \
    .type = MINSK_SYNTAX_NODE_TYPE_COMPILATION_UNIT, \
    .compilation_unit = {__VA_ARGS__}, \
  })
