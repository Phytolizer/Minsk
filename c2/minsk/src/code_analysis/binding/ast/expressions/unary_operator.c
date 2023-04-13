#include "minsk/code_analysis/binding/ast/expressions/unary_operator.h"

#include "minsk/data_structures/buf.h"

#define UNIFORM(sk, k, t)                                              \
  {                                                                    \
    .syntax_kind = sk, .kind = k, .operand_type = t, .result_type = t, \
  }

static minsk_bound_expression_unary_operator_t const ops[] = {
  UNIFORM(
    MINSK_SYNTAX_KIND_PLUS_TOKEN,
    MINSK_BOUND_EXPRESSION_UNARY_OPERATOR_KIND_IDENTITY,
    MINSK_OBJECT_TYPE_INTEGER
  ),
  UNIFORM(
    MINSK_SYNTAX_KIND_MINUS_TOKEN,
    MINSK_BOUND_EXPRESSION_UNARY_OPERATOR_KIND_NEGATION,
    MINSK_OBJECT_TYPE_INTEGER
  ),
  UNIFORM(
    MINSK_SYNTAX_KIND_BANG_TOKEN,
    MINSK_BOUND_EXPRESSION_UNARY_OPERATOR_KIND_LOGICAL_NEGATION,
    MINSK_OBJECT_TYPE_BOOLEAN
  ),
};
typedef BUF_T(minsk_bound_expression_unary_operator_t const, unary_operator)
  operator_buf_t;
static operator_buf_t ops_buf = BUF_ARRAY(operator_buf_t, ops);

extern minsk_bound_expression_unary_operator_t const *
minsk_bound_expression_unary_operator_bind(
  minsk_syntax_kind_t syntax_kind,
  minsk_object_type_t operand_type
)
{
  for (size_t i = 0; i < ops_buf.len; i++)
  {
    minsk_bound_expression_unary_operator_t const * op = &ops_buf.ptr[i];
    if (op->syntax_kind == syntax_kind && op->operand_type == operand_type)
    {
      return op;
    }
  }
  return NULL;
}
