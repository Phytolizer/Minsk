#include "minsk/code_analysis/binding/binder.h"

#include <minsk-platform/debugger.h>

extern minsk_binder_t minsk_binder_new(Arena * arena)
{
  return (minsk_binder_t){
    ._arena = arena,
  };
}

static minsk_bound_node_t bind_binary_expression(
  minsk_binder_t * binder,
  minsk_syntax_expression_binary_t syntax
);
static minsk_bound_node_t bind_literal_expression(
  minsk_syntax_expression_literal_t syntax
);
static minsk_bound_node_t bind_parenthesized_expression(
  minsk_binder_t * binder,
  minsk_syntax_expression_parenthesized_t syntax
);
static minsk_bound_node_t bind_unary_expression(
  minsk_binder_t * binder,
  minsk_syntax_expression_unary_t syntax
);

extern minsk_bound_node_t minsk_binder_bind_expression(
  minsk_binder_t * binder,
  minsk_syntax_node_t syntax
)
{
  switch (syntax.type)
  {
    case MINSK_SYNTAX_NODE_TYPE_BINARY_EXPRESSION:
      return bind_binary_expression(binder, syntax.expression.binary);
    case MINSK_SYNTAX_NODE_TYPE_LITERAL_EXPRESSION:
      return bind_literal_expression(syntax.expression.literal);
    case MINSK_SYNTAX_NODE_TYPE_PARENTHESIZED_EXPRESSION:
      return bind_parenthesized_expression(
        binder,
        syntax.expression.parenthesized
      );
    case MINSK_SYNTAX_NODE_TYPE_UNARY_EXPRESSION:
      return bind_unary_expression(binder, syntax.expression.unary);
    case MINSK_SYNTAX_NODE_TYPE_TOKEN:
    default: DEBUGGER_FATAL("invalid syntax node type %d", syntax.type);
  }
}

static minsk_bound_node_t bind_binary_expression(
  minsk_binder_t * binder,
  minsk_syntax_expression_binary_t syntax
)
{
  minsk_bound_node_t left = minsk_binder_bind_expression(binder, *syntax.left);
  minsk_bound_node_t right =
    minsk_binder_bind_expression(binder, *syntax.right);
  minsk_bound_expression_binary_operator_t const * op =
    minsk_bound_expression_binary_operator_bind(
      syntax.op.kind,
      minsk_bound_expression_get_resolved_type(left.expression),
      minsk_bound_expression_get_resolved_type(right.expression)
    );
  if (op == NULL)
  {
    BUF_PUSH_ARENA(
      binder->_arena,
      &binder->diagnostics,
      string_printf_arena(
        binder->_arena,
        "Binary operator '" STRING_FMT "' is not defined for types " STRING_FMT
        " and " STRING_FMT ".",
        STRING_ARG(syntax.op.text),
        STRING_ARG(minsk_object_type_display_name(
          binder->_arena,
          minsk_bound_expression_get_resolved_type(left.expression)
        )),
        STRING_ARG(minsk_object_type_display_name(
          binder->_arena,
          minsk_bound_expression_get_resolved_type(right.expression)
        ))
      )
    );
    return left;
  }
  return MINSK_BOUND_EXPRESSION_BINARY(
      .left = minsk_bound_node_promote(binder->_arena, left),
      .op = *op,
      .right = minsk_bound_node_promote(binder->_arena, right),
  );
}

static minsk_bound_node_t bind_literal_expression(
  minsk_syntax_expression_literal_t syntax
)
{
  minsk_object_t value = syntax.value;
  if (value.type == MINSK_OBJECT_TYPE_NIL)
  {
    value = MINSK_OBJECT_INTEGER(0);
  }
  return MINSK_BOUND_EXPRESSION_LITERAL(.value = value);
}

static minsk_bound_node_t bind_parenthesized_expression(
  minsk_binder_t * binder,
  minsk_syntax_expression_parenthesized_t syntax
)
{
  return minsk_binder_bind_expression(binder, *syntax.expression);
}

static minsk_bound_node_t bind_unary_expression(
  minsk_binder_t * binder,
  minsk_syntax_expression_unary_t syntax
)
{
  minsk_bound_node_t operand =
    minsk_binder_bind_expression(binder, *syntax.operand);
  minsk_bound_expression_unary_operator_t const * op =
    minsk_bound_expression_unary_operator_bind(
      syntax.op.kind,
      minsk_bound_expression_get_resolved_type(operand.expression)
    );
  if (op == NULL)
  {
    BUF_PUSH_ARENA(
      binder->_arena,
      &binder->diagnostics,
      string_printf_arena(
        binder->_arena,
        "Unary operator '" STRING_FMT "' is not defined for type " STRING_FMT
        ".",
        STRING_ARG(syntax.op.text),
        STRING_ARG(minsk_object_type_display_name(
          binder->_arena,
          minsk_bound_expression_get_resolved_type(operand.expression)
        ))
      )
    );
    return operand;
  }
  return MINSK_BOUND_EXPRESSION_UNARY(
      .op = *op,
      .operand = minsk_bound_node_promote(binder->_arena, operand)
  );
}
