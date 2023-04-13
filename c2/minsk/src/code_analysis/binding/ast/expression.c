#include "minsk/code_analysis/binding/ast/expression.h"

#include <minsk-platform/debugger.h>

#include "minsk/code_analysis/binding/ast/node.h"

extern minsk_object_type_t minsk_bound_expression_get_resolved_type(
  minsk_bound_expression_t expression
)
{
  switch (expression.type)
  {
    case MINSK_BOUND_NODE_TYPE_BINARY_EXPRESSION:
    {
      minsk_bound_expression_binary_t b = expression.binary;
      return b.op.result_type;
    }
    case MINSK_BOUND_NODE_TYPE_LITERAL_EXPRESSION:
    {
      minsk_bound_expression_literal_t l = expression.literal;
      return l.value.type;
    }
    case MINSK_BOUND_NODE_TYPE_UNARY_EXPRESSION:
    {
      minsk_bound_expression_unary_t u = expression.unary;
      return u.op.result_type;
    }
  }

  DEBUGGER_FATAL("bad bound expression type %d", expression.type);
}
