#pragma once

#include "minsk/code_analysis/syntax/token.h"

typedef struct
{
  minsk_syntax_token_t open_parenthesis_token;
  struct minsk_syntax_node* expression;
  minsk_syntax_token_t close_parenthesis_token;
} minsk_syntax_expression_parenthesized_t;

#define MINSK_SYNTAX_EXPRESSION_PARENTHESIZED(...)   \
  MINSK_SYNTAX_EXPRESSION(                           \
    MINSK_SYNTAX_NODE_TYPE_PARENTHESIZED_EXPRESSION, \
    .parenthesized = {__VA_ARGS__}                   \
  )
