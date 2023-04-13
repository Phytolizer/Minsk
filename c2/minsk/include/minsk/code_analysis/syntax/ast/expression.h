#pragma once

#include "./expressions/binary.h"
#include "./expressions/literal.h"
#include "./expressions/parenthesized.h"
#include "./expressions/unary.h"
#include "./node_type.h"

typedef struct
{
  minsk_syntax_node_type_t type;

  union
  {
    minsk_syntax_expression_binary_t binary;
    minsk_syntax_expression_literal_t literal;
    minsk_syntax_expression_parenthesized_t parenthesized;
    minsk_syntax_expression_unary_t unary;
  };
} minsk_syntax_expression_t;

#define MINSK_SYNTAX_EXPRESSION(ty, ...)      \
 ((minsk_syntax_node_t){                      \
   .type = (ty),                              \
   .expression = {.type = (ty), __VA_ARGS__}, \
 })
