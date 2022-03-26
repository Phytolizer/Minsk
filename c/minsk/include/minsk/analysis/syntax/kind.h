#pragma once

#include <stdio.h>
#define SYNTAX_KINDS_X                                                         \
  X(bad_token)                                                                 \
  X(end_of_file_token)                                                         \
                                                                               \
  X(identifier_token)                                                          \
  X(number_token)                                                              \
  X(whitespace_token)                                                          \
                                                                               \
  X(plus_token)                                                                \
  X(minus_token)                                                               \
  X(star_token)                                                                \
  X(slash_token)                                                               \
  X(bang_token)                                                                \
  X(ampersand_ampersand_token)                                                 \
  X(pipe_pipe_token)                                                           \
  X(bang_equals_token)                                                         \
  X(equals_equals_token)                                                       \
  X(open_parenthesis_token)                                                    \
  X(close_parenthesis_token)                                                   \
                                                                               \
  X(true_keyword)                                                              \
  X(false_keyword)                                                             \
                                                                               \
  X(binary_expression)                                                         \
  X(literal_expression)                                                        \
  X(parenthesized_expression)                                                  \
  X(unary_expression)

typedef enum {
#define X(x) syntax_kind_##x,
  SYNTAX_KINDS_X
#undef X
} syntax_kind_t;

void syntax_kind_print(syntax_kind_t kind, FILE *stream);
const char *syntax_kind_to_string(syntax_kind_t kind);
