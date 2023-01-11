#include "minsk/analysis/evaluator.h"
#include "minsk/analysis/binding/kind.h"
#include "minsk/analysis/binding/node/expression.h"
#include "minsk/analysis/binding/node/expression/assignment.h"
#include "minsk/analysis/binding/node/expression/binary.h"
#include "minsk/analysis/binding/node/expression/binary/kind.h"
#include "minsk/analysis/binding/node/expression/literal.h"
#include "minsk/analysis/binding/node/expression/unary.h"
#include "minsk/analysis/binding/node/expression/unary/kind.h"
#include "minsk/analysis/binding/node/expression/variable.h"
#include "minsk/analysis/syntax/kind.h"
#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/node/expression/binary.h"
#include "minsk/analysis/syntax/node/expression/literal.h"
#include "minsk/analysis/syntax/node/expression/parenthesized.h"
#include "minsk/analysis/syntax/node/expression/unary.h"
#include "minsk/analysis/variables.h"
#include "minsk/runtime/object.h"
#include <assert.h>

static object_t*
evaluate_expression(evaluator_t* evaluator, const bound_expression_t* root);

static object_t* evaluate_assignment_expression(
    evaluator_t* evaluator,
    const bound_assignment_expression_t* root
) {
  object_t* value = evaluate_expression(evaluator, root->expression);
  variable_map_insert(
      evaluator->variables,
      variable_symbol_copy(&root->variable),
      object_copy(value)
  );
  return value;
}

static object_t* evaluate_binary_expression(
    evaluator_t* evaluator,
    const bound_binary_expression_t* root
) {
  object_t* left = evaluate_expression(evaluator, root->left);
  object_t* right = evaluate_expression(evaluator, root->right);

  switch (root->op->kind) {
  case bound_binary_operator_kind_addition: {
    int left_value = ((integer_t*)left)->value;
    int right_value = ((integer_t*)right)->value;
    object_free(left);
    object_free(right);
    return integer_new(left_value + right_value);
  } break;
  case bound_binary_operator_kind_subtraction: {
    int left_value = ((integer_t*)left)->value;
    int right_value = ((integer_t*)right)->value;
    object_free(left);
    object_free(right);
    return integer_new(left_value - right_value);
  } break;
  case bound_binary_operator_kind_multiplication: {
    int left_value = ((integer_t*)left)->value;
    int right_value = ((integer_t*)right)->value;
    object_free(left);
    object_free(right);
    return integer_new(left_value * right_value);
  } break;
  case bound_binary_operator_kind_division: {
    int left_value = ((integer_t*)left)->value;
    int right_value = ((integer_t*)right)->value;
    object_free(left);
    object_free(right);
    return integer_new(left_value / right_value);
  } break;
  case bound_binary_operator_kind_logical_and: {
    bool left_value = ((boolean_t*)left)->value;
    bool right_value = ((boolean_t*)right)->value;
    object_free(left);
    object_free(right);
    return boolean_new(left_value && right_value);
  } break;
  case bound_binary_operator_kind_logical_or: {
    bool left_value = ((boolean_t*)left)->value;
    bool right_value = ((boolean_t*)right)->value;
    object_free(left);
    object_free(right);
    return boolean_new(left_value || right_value);
  } break;
  case bound_binary_operator_kind_equality: {
    bool result = object_equals(left, right);
    object_free(left);
    object_free(right);
    return boolean_new(result);
  } break;
  case bound_binary_operator_kind_inequality: {
    bool result = !object_equals(left, right);
    object_free(left);
    object_free(right);
    return boolean_new(result);
  } break;
  default:
    assert(false && "corrupt binary operator");
    return NULL;
  }
}

static object_t* evaluate_literal_expression(
    evaluator_t* evaluator,
    const bound_literal_expression_t* root
) {
  (void)evaluator;
  return object_copy(root->value);
}

static object_t* evaluate_unary_expression(
    evaluator_t* evaluator,
    const bound_unary_expression_t* root
) {
  object_t* operand = evaluate_expression(evaluator, root->operand);

  switch (root->op->kind) {
  case bound_unary_operator_kind_identity: {
    int operand_value = ((integer_t*)operand)->value;
    object_free(operand);
    return integer_new(operand_value);
  } break;
  case bound_unary_operator_kind_negation: {
    int operand_value = ((integer_t*)operand)->value;
    object_free(operand);
    return integer_new(-operand_value);
  } break;
  case bound_unary_operator_kind_logical_negation: {
    bool operand_value = ((boolean_t*)operand)->value;
    object_free(operand);
    return boolean_new(!operand_value);
  } break;
  default:
    assert(false && "corrupt unary operator");
    return NULL;
  }
}

static object_t* evaluate_variable_expression(
    evaluator_t* evaluator,
    const bound_variable_expression_t* root
) {
  variable_map_bucket_t* bucket =
      variable_map_find(evaluator->variables, root->variable);
  assert(bucket != NULL);
  return object_copy(bucket->value);
}

static object_t*
evaluate_expression(evaluator_t* evaluator, const bound_expression_t* root) {
  switch (root->base.kind) {
  case bound_node_kind_assignment_expression:
    return evaluate_assignment_expression(
        evaluator,
        (const bound_assignment_expression_t*)root
    );
  case bound_node_kind_binary_expression:
    return evaluate_binary_expression(
        evaluator,
        (bound_binary_expression_t*)root
    );
  case bound_node_kind_literal_expression:
    return evaluate_literal_expression(
        evaluator,
        (bound_literal_expression_t*)root
    );
  case bound_node_kind_unary_expression:
    return evaluate_unary_expression(
        evaluator,
        (bound_unary_expression_t*)root
    );
  case bound_node_kind_variable_expression:
    return evaluate_variable_expression(
        evaluator,
        (bound_variable_expression_t*)root
    );
  }
  assert(false && "unexpected expression syntax");
  return NULL;
}

void evaluator_init(
    evaluator_t* evaluator,
    const bound_expression_t* root,
    variable_map_t* variables
) {
  evaluator->root = root;
  evaluator->variables = variables;
}

object_t* evaluator_evaluate(evaluator_t* evaluator) {
  return evaluate_expression(evaluator, evaluator->root);
}
