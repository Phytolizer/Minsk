#include "minsk/analysis/evaluator.h"
#include "minsk/analysis/syntax/kind.h"
#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/node/expression/binary.h"
#include "minsk/analysis/syntax/node/expression/literal.h"
#include "minsk/analysis/syntax/node/expression/parenthesized.h"
#include "minsk/analysis/syntax/node/expression/unary.h"
#include "minsk/runtime/object.h"
#include <assert.h>

static object_t *evaluate_expression(expression_syntax_t *root);

static object_t *evaluate_binary_expression(binary_expression_syntax_t *root) {
  object_t *left = evaluate_expression(root->left);
  object_t *right = evaluate_expression(root->right);

  switch (root->operator_token.base.kind) {
  case syntax_kind_plus_token: {
    int left_value = ((integer_t *)left)->value;
    int right_value = ((integer_t *)right)->value;
    object_free(left);
    object_free(right);
    return integer_new(left_value + right_value);
  } break;
  case syntax_kind_minus_token: {
    int left_value = ((integer_t *)left)->value;
    int right_value = ((integer_t *)right)->value;
    object_free(left);
    object_free(right);
    return integer_new(left_value - right_value);
  } break;
  case syntax_kind_star_token: {
    int left_value = ((integer_t *)left)->value;
    int right_value = ((integer_t *)right)->value;
    object_free(left);
    object_free(right);
    return integer_new(left_value * right_value);
  } break;
  case syntax_kind_slash_token: {
    int left_value = ((integer_t *)left)->value;
    int right_value = ((integer_t *)right)->value;
    object_free(left);
    object_free(right);
    return integer_new(left_value / right_value);
  } break;
  default:
    assert(false && "unexpected binary operator");
    return NULL;
  }
}

static object_t *
evaluate_literal_expression(literal_expression_syntax_t *root) {
  return object_copy(root->value);
}

static object_t *
evaluate_parenthesized_expression(parenthesized_expression_syntax_t *root) {
  return evaluate_expression(root->expression);
}

static object_t *evaluate_unary_expression(unary_expression_syntax_t *root) {
  object_t *operand = evaluate_expression(root->operand);

  switch (root->operator_token.base.kind) {
  case syntax_kind_plus_token: {
    int operand_value = ((integer_t *)operand)->value;
    object_free(operand);
    return integer_new(operand_value);
  } break;
  case syntax_kind_minus_token: {
    int operand_value = ((integer_t *)operand)->value;
    object_free(operand);
    return integer_new(-operand_value);
  } break;
  default:
    assert(false && "unexpected unary operator");
    return NULL;
  }
}

static object_t *evaluate_expression(expression_syntax_t *root) {
  switch (root->base.kind) {
  case syntax_kind_binary_expression:
    return evaluate_binary_expression((binary_expression_syntax_t *)root);
  case syntax_kind_literal_expression:
    return evaluate_literal_expression((literal_expression_syntax_t *)root);
  case syntax_kind_parenthesized_expression:
    return evaluate_parenthesized_expression(
        (parenthesized_expression_syntax_t *)root);
  case syntax_kind_unary_expression:
    return evaluate_unary_expression((unary_expression_syntax_t *)root);
  default:
    assert(false && "unexpected expression syntax");
    return NULL;
  }
}

void evaluator_init(evaluator_t *evaluator, expression_syntax_t *root) {
  evaluator->root = root;
}

object_t *evaluator_evaluate(evaluator_t *evaluator) {
  return evaluate_expression(evaluator->root);
}
