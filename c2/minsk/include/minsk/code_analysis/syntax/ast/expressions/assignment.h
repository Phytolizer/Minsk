#pragma once

#include "minsk/code_analysis/syntax/token.h"

typedef struct
{
  minsk_syntax_token_t identifier_token;
  minsk_syntax_token_t equals_token;
  struct minsk_syntax_node * expression;
} minsk_syntax_expression_assignment_t;

#define MINSK_SYNTAX_EXPRESSION_ASSIGNMENT(...)   \
  MINSK_SYNTAX_EXPRESSION(                        \
    MINSK_SYNTAX_NODE_TYPE_ASSIGNMENT_EXPRESSION, \
    .assignment = {__VA_ARGS__}                   \
  )
