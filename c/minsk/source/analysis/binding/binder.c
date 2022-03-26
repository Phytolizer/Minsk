#include "minsk/analysis/binding/binder.h"
#include "minsk/analysis/binding/node/expression.h"
#include "minsk/analysis/binding/node/expression/assignment.h"
#include "minsk/analysis/binding/node/expression/binary.h"
#include "minsk/analysis/binding/node/expression/binary/operator.h"
#include "minsk/analysis/binding/node/expression/literal.h"
#include "minsk/analysis/binding/node/expression/unary.h"
#include "minsk/analysis/binding/node/expression/unary/operator.h"
#include "minsk/analysis/binding/node/expression/variable.h"
#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/symbol.h"
#include "minsk/analysis/syntax/kind.h"
#include "minsk/analysis/syntax/node/expression/assignment.h"
#include "minsk/analysis/syntax/node/expression/binary.h"
#include "minsk/analysis/syntax/node/expression/literal.h"
#include "minsk/analysis/syntax/node/expression/name.h"
#include "minsk/analysis/syntax/node/expression/parenthesized.h"
#include "minsk/analysis/syntax/node/expression/unary.h"
#include "minsk/analysis/syntax/token.h"
#include "minsk/analysis/variables.h"
#include "minsk/runtime/object.h"
#include <assert.h>

void binder_init(binder_t *binder, variable_map_t *variables) {
  diagnostic_bag_init(&binder->diagnostics);
  binder->variables = variables;
}

static bound_expression_t *
bind_binary_expression(binder_t *binder,
                       const binary_expression_syntax_t *syntax) {
  bound_expression_t *left = binder_bind_expression(binder, syntax->left);
  bound_expression_t *right = binder_bind_expression(binder, syntax->right);
  const bound_binary_operator_t *op = bound_binary_operator_bind(
      syntax->operator_token.base.kind, bound_expression_type(left),
      bound_expression_type(right));
  if (op == NULL) {
    diagnostic_bag_report_undefined_binary_operator(
        &binder->diagnostics, token_span(&syntax->operator_token),
        syntax->operator_token.text, bound_expression_type(left),
        bound_expression_type(right));
  }
  bound_expression_t *expression = bound_binary_expression_new(left, op, right);
  return expression;
}

static bound_expression_t *
bind_literal_expression(binder_t *binder,
                        const literal_expression_syntax_t *syntax) {
  (void)binder;
  bound_expression_t *expression =
      bound_literal_expression_new(object_copy(syntax->value));
  return expression;
}

static bound_expression_t *
bind_assignment_expression(binder_t *binder,
                           const assignment_expression_syntax_t *syntax) {
  bound_expression_t *expression =
      binder_bind_expression(binder, syntax->expression);
  variable_symbol_t variable;
  variable_symbol_init(&variable, sdsdup(syntax->identifier_token.text),
                       bound_expression_type(expression));
  variable_map_insert(binder->variables, variable, NULL);
  bound_expression_t *result = bound_assignment_expression_new(
      variable_symbol_copy(&variable), expression);
  return result;
}

static bound_expression_t *
bind_name_expression(binder_t *binder, const name_expression_syntax_t *syntax) {
  (void)binder;
  variable_map_bucket_t *bucket = variable_map_find_by_name(
      binder->variables, syntax->identifier_token.text);
  if (bucket == NULL) {
    diagnostic_bag_report_undefined_variable(
        &binder->diagnostics, token_span(&syntax->identifier_token),
        syntax->identifier_token.text);
    return bound_literal_expression_new(integer_new(0));
  }
  return bound_variable_expression_new(variable_symbol_copy(&bucket->variable));
}

static bound_expression_t *
bind_parenthesized_expression(binder_t *binder,
                              const parenthesized_expression_syntax_t *syntax) {
  bound_expression_t *expression =
      binder_bind_expression(binder, syntax->expression);
  return expression;
}

static bound_expression_t *
bind_unary_expression(binder_t *binder,
                      const unary_expression_syntax_t *syntax) {
  bound_expression_t *operand = binder_bind_expression(binder, syntax->operand);
  const bound_unary_operator_t *op = bound_unary_operator_bind(
      syntax->operator_token.base.kind, bound_expression_type(operand));
  if (op == NULL) {
    diagnostic_bag_report_undefined_unary_operator(
        &binder->diagnostics, token_span(&syntax->operator_token),
        syntax->operator_token.text, bound_expression_type(operand));
  }
  bound_expression_t *expression = bound_unary_expression_new(op, operand);
  return expression;
}

bound_expression_t *
binder_bind_expression(binder_t *binder,
                       const expression_syntax_t *expression) {
  switch (expression->base.kind) {
  case syntax_kind_assignment_expression:
    return bind_assignment_expression(
        binder, (const assignment_expression_syntax_t *)expression);
  case syntax_kind_binary_expression:
    return bind_binary_expression(
        binder, (const binary_expression_syntax_t *)expression);
  case syntax_kind_literal_expression:
    return bind_literal_expression(
        binder, (const literal_expression_syntax_t *)expression);
  case syntax_kind_name_expression:
    return bind_name_expression(binder,
                                (const name_expression_syntax_t *)expression);
  case syntax_kind_parenthesized_expression:
    return bind_parenthesized_expression(
        binder, (const parenthesized_expression_syntax_t *)expression);
  case syntax_kind_unary_expression:
    return bind_unary_expression(binder,
                                 (const unary_expression_syntax_t *)expression);
  default:
    assert(false && "corrupt syntax node");
  }
}

void binder_free(binder_t *binder) {
  diagnostic_bag_free(&binder->diagnostics);
}
