#include "minsk/code_analysis/evaluator.h"

#include <minsk-platform/debugger.h>

#include "minsk/code_analysis/binding/ast/expression.h"
#include "minsk/code_analysis/binding/ast/expressions/binary.h"
#include "minsk/code_analysis/binding/ast/expressions/literal.h"
#include "minsk/code_analysis/binding/ast/node.h"
#include "minsk/code_analysis/binding/ast/node_type.h"

static minsk_object_t evaluate_expression(minsk_bound_node_t const * node);
static minsk_object_t evaluate_binary_expression(
  minsk_bound_expression_binary_t node
);
static minsk_object_t evaluate_literal_expression(
  minsk_bound_expression_literal_t node
);
static minsk_object_t evaluate_unary_expression(
  minsk_bound_expression_unary_t node
);

extern minsk_evaluator_t minsk_evaluator_new(minsk_bound_node_t root)
{
  return (minsk_evaluator_t){
    ._root = root,
  };
}

extern minsk_object_t minsk_evaluator_evaluate(minsk_evaluator_t * evaluator)
{
  return evaluate_expression(&evaluator->_root);
}

static minsk_object_t evaluate_expression(minsk_bound_node_t const * node)
{
  switch (node->type)
  {
    case MINSK_BOUND_NODE_TYPE_BINARY_EXPRESSION:
      return evaluate_binary_expression(node->expression.binary);
    case MINSK_BOUND_NODE_TYPE_LITERAL_EXPRESSION:
      return evaluate_literal_expression(node->expression.literal);
    case MINSK_BOUND_NODE_TYPE_UNARY_EXPRESSION:
      return evaluate_unary_expression(node->expression.unary);
  }
  DEBUGGER_FATAL("invalid bound node type %d", node->type);
}

static minsk_object_t evaluate_binary_expression(
  minsk_bound_expression_binary_t node
)
{
  minsk_object_t left = evaluate_expression(node.left);
  minsk_object_t right = evaluate_expression(node.right);

  switch (node.op.kind)
  {
    case MINSK_BOUND_EXPRESSION_BINARY_OPERATOR_KIND_ADDITION:
      return MINSK_OBJECT_INTEGER(left.integer + right.integer);
    case MINSK_BOUND_EXPRESSION_BINARY_OPERATOR_KIND_SUBTRACTION:
      return MINSK_OBJECT_INTEGER(left.integer - right.integer);
    case MINSK_BOUND_EXPRESSION_BINARY_OPERATOR_KIND_MULTIPLICATION:
      return MINSK_OBJECT_INTEGER(left.integer * right.integer);
    case MINSK_BOUND_EXPRESSION_BINARY_OPERATOR_KIND_DIVISION:
      return MINSK_OBJECT_INTEGER(left.integer / right.integer);
    case MINSK_BOUND_EXPRESSION_BINARY_OPERATOR_KIND_LOGICAL_AND:
      return MINSK_OBJECT_BOOLEAN(left.boolean && right.boolean);
    case MINSK_BOUND_EXPRESSION_BINARY_OPERATOR_KIND_LOGICAL_OR:
      return MINSK_OBJECT_BOOLEAN(left.boolean || right.boolean);
    case MINSK_BOUND_EXPRESSION_BINARY_OPERATOR_KIND_EQUALITY:
      switch (left.type)
      {
        case MINSK_OBJECT_TYPE_BOOLEAN:
          return MINSK_OBJECT_BOOLEAN(left.boolean == right.boolean);
        case MINSK_OBJECT_TYPE_INTEGER:
          return MINSK_OBJECT_BOOLEAN(left.integer == right.integer);
        default: DEBUGGER_FATAL("unreachable");
      }
    case MINSK_BOUND_EXPRESSION_BINARY_OPERATOR_KIND_INEQUALITY:
      switch (left.type)
      {
        case MINSK_OBJECT_TYPE_BOOLEAN:
          return MINSK_OBJECT_BOOLEAN(left.boolean != right.boolean);
        case MINSK_OBJECT_TYPE_INTEGER:
          return MINSK_OBJECT_BOOLEAN(left.integer != right.integer);
        default: DEBUGGER_FATAL("unreachable");
      }
    default: DEBUGGER_FATAL("invalid binary operator %d", node.op.kind);
  }
}

static minsk_object_t evaluate_literal_expression(
  minsk_bound_expression_literal_t node
)
{
  return node.value;
}

static minsk_object_t evaluate_unary_expression(
  minsk_bound_expression_unary_t node
)
{
  minsk_object_t operand = evaluate_expression(node.operand);

  switch (node.op.kind)
  {
    case MINSK_BOUND_EXPRESSION_UNARY_OPERATOR_KIND_IDENTITY: return operand;
    case MINSK_BOUND_EXPRESSION_UNARY_OPERATOR_KIND_NEGATION:
      return MINSK_OBJECT_INTEGER(-operand.integer);
    case MINSK_BOUND_EXPRESSION_UNARY_OPERATOR_KIND_LOGICAL_NEGATION:
      return MINSK_OBJECT_BOOLEAN(!operand.boolean);
    default: DEBUGGER_FATAL("invalid unary operator %d", node.op.kind);
  }
}
