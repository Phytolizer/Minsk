#include "minsk/code_analysis/evaluator.h"

#include <minsk-platform/debugger.h>

static minsk_object_t evaluate_expression(minsk_syntax_node_t const* root);
static minsk_object_t evaluate_literal_expression(
  minsk_syntax_expression_literal_t syntax
);
static minsk_object_t evaluate_binary_expression(
  minsk_syntax_expression_binary_t syntax
);

extern minsk_evaluator_t minsk_evaluator_new(minsk_syntax_node_t root)
{
  return (minsk_evaluator_t){
    ._root = root,
  };
}
extern minsk_object_t minsk_evaluator_evaluate(minsk_evaluator_t* evaluator)
{
  return evaluate_expression(&evaluator->_root);
}

static minsk_object_t evaluate_expression(minsk_syntax_node_t const* root)
{
  switch (root->type)
  {
    case MINSK_SYNTAX_NODE_TYPE_LITERAL_EXPRESSION:
      return evaluate_literal_expression(root->expression.literal);
    case MINSK_SYNTAX_NODE_TYPE_BINARY_EXPRESSION:
      return evaluate_binary_expression(root->expression.binary);
    case MINSK_SYNTAX_NODE_TYPE_TOKEN:
    default: DEBUGGER_FATAL("invalid syntax node type %d", root->type);
  }
}

static minsk_object_t evaluate_literal_expression(
  minsk_syntax_expression_literal_t syntax
)
{
  return syntax.literal_token.value;
}

static minsk_object_t evaluate_binary_expression(
  minsk_syntax_expression_binary_t syntax
)
{
  minsk_object_t left = evaluate_expression(syntax.left);
  minsk_object_t right = evaluate_expression(syntax.right);

  switch (syntax.op.kind)
  {
    case MINSK_SYNTAX_KIND_PLUS_TOKEN:
      return MINSK_OBJECT_INTEGER(left.integer + right.integer);
    case MINSK_SYNTAX_KIND_MINUS_TOKEN:
      return MINSK_OBJECT_INTEGER(left.integer - right.integer);
    case MINSK_SYNTAX_KIND_STAR_TOKEN:
      return MINSK_OBJECT_INTEGER(left.integer * right.integer);
    case MINSK_SYNTAX_KIND_SLASH_TOKEN:
      return MINSK_OBJECT_INTEGER(left.integer / right.integer);
    default: DEBUGGER_FATAL("invalid binary operator %d", syntax.op.kind);
  }
}
