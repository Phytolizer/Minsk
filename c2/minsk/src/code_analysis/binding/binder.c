#include "minsk/code_analysis/binding/binder.h"

#include <arena.h>
#include <minsk-platform/debugger.h>
#include <minsk-string/string.h>
#include <stddef.h>

#include "minsk/code_analysis/binding/ast/expression.h"
#include "minsk/code_analysis/binding/ast/expressions/binary_operator.h"
#include "minsk/code_analysis/binding/ast/expressions/unary_operator.h"
#include "minsk/code_analysis/syntax/ast/expression.h"
#include "minsk/code_analysis/syntax/ast/expressions/binary.h"
#include "minsk/code_analysis/syntax/ast/expressions/literal.h"
#include "minsk/code_analysis/syntax/ast/expressions/parenthesized.h"
#include "minsk/code_analysis/syntax/ast/expressions/unary.h"
#include "minsk/code_analysis/syntax/ast/node_type.h"
#include "minsk/code_analysis/syntax/token.h"
#include "minsk/data_structures/buf.h"
#include "minsk/runtime/object.h"

extern minsk_binder_t
minsk_binder_new(Arena * arena, minsk_variable_map_t * variables)
{
  return (minsk_binder_t){
    ._arena = arena,
    ._variables = variables,
    .diagnostics = minsk_diagnostic_bag_new(arena),
  };
}

static minsk_bound_node_t
bind_assignment_expression(
  minsk_binder_t * binder,
  minsk_syntax_expression_assignment_t syntax
);
static minsk_bound_node_t
bind_binary_expression(
  minsk_binder_t * binder,
  minsk_syntax_expression_binary_t syntax
);
static minsk_bound_node_t
bind_literal_expression(minsk_syntax_expression_literal_t syntax);
static minsk_bound_node_t
bind_name_expression(
  minsk_binder_t * binder,
  minsk_syntax_expression_name_t syntax
);
static minsk_bound_node_t
bind_parenthesized_expression(
  minsk_binder_t * binder,
  minsk_syntax_expression_parenthesized_t syntax
);
static minsk_bound_node_t
bind_unary_expression(
  minsk_binder_t * binder,
  minsk_syntax_expression_unary_t syntax
);

extern minsk_bound_node_t
minsk_binder_bind_expression(
  minsk_binder_t * binder,
  minsk_syntax_node_t syntax
)
{
  switch (syntax.type)
  {
    case MINSK_SYNTAX_NODE_TYPE_ASSIGNMENT_EXPRESSION:
      return bind_assignment_expression(binder, syntax.expression.assignment);
    case MINSK_SYNTAX_NODE_TYPE_BINARY_EXPRESSION:
      return bind_binary_expression(binder, syntax.expression.binary);
    case MINSK_SYNTAX_NODE_TYPE_LITERAL_EXPRESSION:
      return bind_literal_expression(syntax.expression.literal);
    case MINSK_SYNTAX_NODE_TYPE_NAME_EXPRESSION:
      return bind_name_expression(binder, syntax.expression.name);
    case MINSK_SYNTAX_NODE_TYPE_PARENTHESIZED_EXPRESSION:
      return bind_parenthesized_expression(
        binder,
        syntax.expression.parenthesized
      );
    case MINSK_SYNTAX_NODE_TYPE_UNARY_EXPRESSION:
      return bind_unary_expression(binder, syntax.expression.unary);
    case MINSK_SYNTAX_NODE_TYPE_TOKEN: break;
  }

  DEBUGGER_FATAL("invalid syntax node type %d", syntax.type);
}

static minsk_bound_node_t
bind_assignment_expression(
  minsk_binder_t * binder,
  minsk_syntax_expression_assignment_t syntax
)
{
  string_t name = syntax.identifier_token.text;
  minsk_bound_node_t expression =
    minsk_binder_bind_expression(binder, *syntax.expression);

  minsk_variable_symbol_t existing_variable;
  if (minsk_variable_map_find_by_name(
        binder->_variables,
        name,
        &existing_variable
      ))
  {
    minsk_variable_map_del(binder->_variables, existing_variable);
  }

  minsk_variable_symbol_t variable = {
    name,
    minsk_bound_expression_get_resolved_type(expression.expression),
  };
  minsk_variable_map_put(binder->_variables, variable, MINSK_OBJECT_NIL);
  return MINSK_BOUND_EXPRESSION_ASSIGNMENT(
      .variable = variable,
      .expression = minsk_bound_node_promote(binder->_arena, expression)
  );
}

static minsk_bound_node_t
bind_binary_expression(
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
    minsk_diagnostic_bag_report_undefined_binary_operator(
      &binder->diagnostics,
      minsk_syntax_token_span(syntax.op),
      syntax.op.text,
      minsk_bound_expression_get_resolved_type(left.expression),
      minsk_bound_expression_get_resolved_type(right.expression)
    );
    return left;
  }
  return MINSK_BOUND_EXPRESSION_BINARY(
      .left = minsk_bound_node_promote(binder->_arena, left),
      .op = *op,
      .right = minsk_bound_node_promote(binder->_arena, right),
  );
}

static minsk_bound_node_t
bind_literal_expression(minsk_syntax_expression_literal_t syntax)
{
  minsk_object_t value = syntax.value;
  if (value.type == MINSK_OBJECT_TYPE_NIL)
  {
    value = MINSK_OBJECT_INTEGER(0);
  }
  return MINSK_BOUND_EXPRESSION_LITERAL(.value = value);
}

static minsk_bound_node_t
bind_name_expression(
  minsk_binder_t * binder,
  minsk_syntax_expression_name_t syntax
)
{
  string_t name = syntax.identifier_token.text;
  minsk_variable_symbol_t variable;
  if (!minsk_variable_map_find_by_name(binder->_variables, name, &variable))
  {
    minsk_diagnostic_bag_report_undefined_name(
      &binder->diagnostics,
      minsk_syntax_token_span(syntax.identifier_token),
      name
    );
    return MINSK_BOUND_EXPRESSION_LITERAL(MINSK_OBJECT_INTEGER(0));
  }

  return MINSK_BOUND_EXPRESSION_VARIABLE(.variable = variable);
}

static minsk_bound_node_t
bind_parenthesized_expression(
  minsk_binder_t * binder,
  minsk_syntax_expression_parenthesized_t syntax
)
{
  return minsk_binder_bind_expression(binder, *syntax.expression);
}

static minsk_bound_node_t
bind_unary_expression(
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
    minsk_diagnostic_bag_report_undefined_unary_operator(
      &binder->diagnostics,
      minsk_syntax_token_span(syntax.op),
      syntax.op.text,
      minsk_bound_expression_get_resolved_type(operand.expression)
    );
    return operand;
  }
  return MINSK_BOUND_EXPRESSION_UNARY(
      .op = *op,
      .operand = minsk_bound_node_promote(binder->_arena, operand)
  );
}
