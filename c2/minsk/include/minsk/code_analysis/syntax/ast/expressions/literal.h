#pragma once

#include "minsk/code_analysis/syntax/token.h"

typedef struct
{
  minsk_syntax_token_t literal_token;
} minsk_syntax_expression_literal_t;

#define MINSK_SYNTAX_EXPRESSION_LITERAL(literal_token_) \
  MINSK_SYNTAX_EXPRESSION(                              \
    MINSK_SYNTAX_NODE_TYPE_LITERAL_EXPRESSION,          \
    .literal = {.literal_token = (literal_token_)}      \
  )
