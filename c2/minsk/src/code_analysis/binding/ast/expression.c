#include "minsk/code_analysis/binding/ast/expression.h"

#include <minsk-platform/debugger.h>

#include "minsk/code_analysis/binding/ast/expressions/binary_operator.h"
#include "minsk/code_analysis/binding/ast/expressions/unary_operator.h"
#include "minsk/code_analysis/binding/ast/node.h"
#include "minsk/code_analysis/variable_symbol.h"

extern minsk_object_type_t
minsk_bound_expression_get_resolved_type(minsk_bound_expression_t expression)
{
  switch (expression.type)
  {
    case MINSK_BOUND_NODE_TYPE_ASSIGNMENT_EXPRESSION:
    {
      minsk_bound_expression_assignment_t a = expression.assignment;
      return minsk_bound_expression_get_resolved_type(a.expression->expression);
    }
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
    case MINSK_BOUND_NODE_TYPE_VARIABLE_EXPRESSION:
    {
      minsk_bound_expression_variable_t v = expression.variable;
      return v.variable.type;
    }
  }

  DEBUGGER_FATAL("bad bound expression type %d", expression.type);
}
