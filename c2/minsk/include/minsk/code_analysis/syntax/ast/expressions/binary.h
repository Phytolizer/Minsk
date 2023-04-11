#pragma once

#include "minsk/code_analysis/syntax/token.h"

typedef struct
{
  struct minsk_syntax_node* left;
  minsk_syntax_token_t op;
  struct minsk_syntax_node* right;
} minsk_syntax_expression_binary_t;

#define MINSK_SYNTAX_EXPRESSION_BINARY(left_, op_, right_)      \
  MINSK_SYNTAX_EXPRESSION(                                      \
    MINSK_SYNTAX_NODE_TYPE_BINARY_EXPRESSION,                   \
    .binary = {.left = (left_), .op = (op_), .right = (right_)} \
  )
