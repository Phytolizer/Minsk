#pragma once

#include "minsk/code_analysis/syntax/token.h"

typedef struct
{
  minsk_syntax_token_t identifier_token;
} minsk_syntax_expression_name_t;

#define MINSK_SYNTAX_EXPRESSION_NAME(...) \
  MINSK_SYNTAX_EXPRESSION( \
    MINSK_SYNTAX_NODE_TYPE_NAME_EXPRESSION, \
    .name = {__VA_ARGS__} \
  )
