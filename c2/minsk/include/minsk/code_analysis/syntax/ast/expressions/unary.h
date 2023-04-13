#pragma once

#include "minsk/code_analysis/syntax/token.h"

typedef struct
{
  minsk_syntax_token_t op;
  struct minsk_syntax_node * operand;
} minsk_syntax_expression_unary_t;

#define MINSK_SYNTAX_EXPRESSION_UNARY(...)  \
 MINSK_SYNTAX_EXPRESSION(                   \
   MINSK_SYNTAX_NODE_TYPE_UNARY_EXPRESSION, \
   .unary = {__VA_ARGS__}                   \
 )
