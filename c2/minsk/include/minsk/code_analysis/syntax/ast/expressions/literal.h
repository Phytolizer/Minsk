#pragma once

#include "minsk/code_analysis/syntax/token.h"

typedef struct
{
  minsk_syntax_token_t literal_token;
  minsk_object_t value;
} minsk_syntax_expression_literal_t;

#define MINSK_SYNTAX_EXPRESSION_LITERAL(...)  \
 MINSK_SYNTAX_EXPRESSION(                     \
   MINSK_SYNTAX_NODE_TYPE_LITERAL_EXPRESSION, \
   .literal = {__VA_ARGS__}                   \
 )
