#include "minsk/code_analysis/binding/ast/expressions/binary_operator.h"

#include <stddef.h>

#include "minsk/data_structures/buf.h"

#define UNIFORM(sk, k, t) \
  { \
    .syntax_kind = sk, .kind = k, .left_type = t, .right_type = t, \
    .result_type = t, \
  }
#define MATCHING(sk, k, ti, to) \
  { \
    .syntax_kind = sk, .kind = k, .left_type = ti, .right_type = ti, \
    .result_type = to, \
  }

static minsk_bound_expression_binary_operator_t const ops[] = {
  UNIFORM(
    MINSK_SYNTAX_KIND_PLUS_TOKEN,
    MINSK_BOUND_EXPRESSION_BINARY_OPERATOR_KIND_ADDITION,
    MINSK_OBJECT_TYPE_INTEGER
  ),
  UNIFORM(
    MINSK_SYNTAX_KIND_MINUS_TOKEN,
    MINSK_BOUND_EXPRESSION_BINARY_OPERATOR_KIND_SUBTRACTION,
    MINSK_OBJECT_TYPE_INTEGER
  ),
  UNIFORM(
    MINSK_SYNTAX_KIND_STAR_TOKEN,
    MINSK_BOUND_EXPRESSION_BINARY_OPERATOR_KIND_MULTIPLICATION,
    MINSK_OBJECT_TYPE_INTEGER
  ),
  UNIFORM(
    MINSK_SYNTAX_KIND_SLASH_TOKEN,
    MINSK_BOUND_EXPRESSION_BINARY_OPERATOR_KIND_DIVISION,
    MINSK_OBJECT_TYPE_INTEGER
  ),
  UNIFORM(
    MINSK_SYNTAX_KIND_AMPERSAND_AMPERSAND_TOKEN,
    MINSK_BOUND_EXPRESSION_BINARY_OPERATOR_KIND_LOGICAL_AND,
    MINSK_OBJECT_TYPE_BOOLEAN
  ),
  UNIFORM(
    MINSK_SYNTAX_KIND_PIPE_PIPE_TOKEN,
    MINSK_BOUND_EXPRESSION_BINARY_OPERATOR_KIND_LOGICAL_OR,
    MINSK_OBJECT_TYPE_BOOLEAN
  ),
  UNIFORM(
    MINSK_SYNTAX_KIND_EQUALS_EQUALS_TOKEN,
    MINSK_BOUND_EXPRESSION_BINARY_OPERATOR_KIND_EQUALITY,
    MINSK_OBJECT_TYPE_BOOLEAN
  ),
  UNIFORM(
    MINSK_SYNTAX_KIND_BANG_EQUALS_TOKEN,
    MINSK_BOUND_EXPRESSION_BINARY_OPERATOR_KIND_INEQUALITY,
    MINSK_OBJECT_TYPE_BOOLEAN
  ),
  MATCHING(
    MINSK_SYNTAX_KIND_EQUALS_EQUALS_TOKEN,
    MINSK_BOUND_EXPRESSION_BINARY_OPERATOR_KIND_EQUALITY,
    MINSK_OBJECT_TYPE_INTEGER,
    MINSK_OBJECT_TYPE_BOOLEAN
  ),
  MATCHING(
    MINSK_SYNTAX_KIND_BANG_EQUALS_TOKEN,
    MINSK_BOUND_EXPRESSION_BINARY_OPERATOR_KIND_INEQUALITY,
    MINSK_OBJECT_TYPE_INTEGER,
    MINSK_OBJECT_TYPE_BOOLEAN
  ),
};
typedef BUF_T(minsk_bound_expression_binary_operator_t const) operator_buf_t;
static operator_buf_t ops_buf = BUF_ARRAY(operator_buf_t, ops);

extern minsk_bound_expression_binary_operator_t const *
minsk_bound_expression_binary_operator_bind(
  minsk_syntax_kind_t syntax_kind,
  minsk_object_type_t left_type,
  minsk_object_type_t right_type
)
{
  for (size_t i = 0; i < ops_buf.len; i++)
  {
    minsk_bound_expression_binary_operator_t const * op = &ops_buf.ptr[i];
    if (op->syntax_kind == syntax_kind && op->left_type == left_type && op->right_type == right_type)
    {
      return op;
    }
  }
  return NULL;
}
