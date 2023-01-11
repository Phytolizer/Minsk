#include "minsk/analysis/binding/binder.h"
#include "minsk/analysis/binding/node/expression.h"
#include "minsk/analysis/binding/node/expression/assignment.h"
#include "minsk/analysis/binding/node/expression/binary.h"
#include "minsk/analysis/binding/node/expression/binary/operator.h"
#include "minsk/analysis/binding/node/expression/literal.h"
#include "minsk/analysis/binding/node/expression/unary.h"
#include "minsk/analysis/binding/node/expression/unary/operator.h"
#include "minsk/analysis/binding/node/expression/variable.h"
#include "minsk/analysis/binding/scope.h"
#include "minsk/analysis/binding/scope/global.h"
#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/symbol.h"
#include "minsk/analysis/syntax/kind.h"
#include "minsk/analysis/syntax/node.h"
#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/analysis/syntax/node/expression/assignment.h"
#include "minsk/analysis/syntax/node/expression/binary.h"
#include "minsk/analysis/syntax/node/expression/literal.h"
#include "minsk/analysis/syntax/node/expression/name.h"
#include "minsk/analysis/syntax/node/expression/parenthesized.h"
#include "minsk/analysis/syntax/node/expression/unary.h"
#include "minsk/analysis/syntax/node/unit.h"
#include "minsk/analysis/syntax/token.h"
#include "minsk/analysis/variables.h"
#include "minsk/runtime/object.h"
#include <assert.h>
#include <stddef.h>
#include <stdlib.h>

void binder_init(binder_t* binder, bound_scope_t* parent) {
  diagnostic_bag_init(&binder->diagnostics);
  binder->scope = malloc(sizeof(bound_scope_t));
  bound_scope_init(binder->scope, parent);
}

typedef struct {
  bound_global_scope_t* data;
  size_t length;
  size_t capacity;
} bound_global_scope_vector_t;

static void bound_global_scope_vector_init(bound_global_scope_vector_t* vector
) {
  vector->data = NULL;
  vector->length = 0;
  vector->capacity = 0;
}

static void bound_global_scope_vector_push(
    bound_global_scope_vector_t* vector,
    bound_global_scope_t value
) {
  if (vector->length == vector->capacity) {
    vector->capacity = vector->capacity == 0 ? 1 : vector->capacity * 2;
    bound_global_scope_t* new_data =
        realloc(vector->data, sizeof(bound_global_scope_t) * vector->capacity);
    assert(new_data != NULL);
    vector->data = new_data;
  }
  vector->data[vector->length] = value;
  vector->length += 1;
}

static bound_global_scope_t
bound_global_scope_vector_pop(bound_global_scope_vector_t* vector) {
  assert(vector->length > 0);
  vector->length -= 1;
  bound_global_scope_t result = vector->data[vector->length];
  return result;
}

static void bound_global_scope_vector_free(bound_global_scope_vector_t* vector
) {
  free(vector->data);
  bound_global_scope_vector_init(vector);
}

static bound_scope_t* create_parent_scope(bound_global_scope_t* previous) {
  bound_global_scope_vector_t stack;
  bound_global_scope_vector_init(&stack);

  while (previous != NULL) {
    bound_global_scope_vector_push(&stack, *previous);
    previous = previous->previous;
  }

  bound_scope_t* current = NULL;

  while (stack.length > 0) {
    bound_global_scope_t scope = bound_global_scope_vector_pop(&stack);
    bound_scope_t* new_scope = malloc(sizeof(bound_scope_t));
    bound_scope_init(new_scope, current);
    for (size_t i = 0; i < scope.variables.length; i++) {
      bound_scope_try_declare(
          new_scope,
          sdsdup(scope.variables.data[i].name),
          scope.variables.data[i]
      );
    }
    current = new_scope;
  }

  bound_global_scope_vector_free(&stack);
  return current;
}

bound_global_scope_t binder_bind_global_scope(
    bound_global_scope_t* previous,
    const compilation_unit_syntax_t* syntax
) {
  bound_scope_t* parent_scope = create_parent_scope(previous);
  binder_t binder;
  binder_init(&binder, parent_scope);
  bound_expression_t* expression =
      binder_bind_expression(&binder, syntax->root);
  variable_symbol_vector_t variables =
      bound_scope_get_declared_variables(binder.scope);
  diagnostic_bag_t diagnostics = binder.diagnostics;

  if (previous != NULL) {
    diagnostic_bag_t combined_diagnostics;
    diagnostic_bag_init(&combined_diagnostics);
    diagnostic_bag_append_range(&combined_diagnostics, previous->diagnostics);
    diagnostic_bag_append_range(&combined_diagnostics, diagnostics);
    diagnostic_bag_free(&diagnostics);
    diagnostics = combined_diagnostics;
  }

  bound_global_scope_t global_scope;
  bound_global_scope_init(
      &global_scope,
      NULL,
      diagnostics,
      variables,
      expression
  );
  binder_free(&binder);
  return global_scope;
}

static bound_expression_t* bind_binary_expression(
    binder_t* binder,
    const binary_expression_syntax_t* syntax
) {
  bound_expression_t* left = binder_bind_expression(binder, syntax->left);
  bound_expression_t* right = binder_bind_expression(binder, syntax->right);
  const bound_binary_operator_t* op = bound_binary_operator_bind(
      syntax->operator_token.base.kind,
      bound_expression_type(left),
      bound_expression_type(right)
  );
  if (op == NULL) {
    diagnostic_bag_report_undefined_binary_operator(
        &binder->diagnostics,
        token_span(&syntax->operator_token),
        syntax->operator_token.text,
        bound_expression_type(left),
        bound_expression_type(right)
    );
  }
  bound_expression_t* expression = bound_binary_expression_new(left, op, right);
  return expression;
}

static bound_expression_t* bind_literal_expression(
    binder_t* binder,
    const literal_expression_syntax_t* syntax
) {
  (void)binder;
  bound_expression_t* expression =
      bound_literal_expression_new(object_copy(syntax->value));
  return expression;
}

static bound_expression_t* bind_assignment_expression(
    binder_t* binder,
    const assignment_expression_syntax_t* syntax
) {
  bound_expression_t* expression =
      binder_bind_expression(binder, syntax->expression);
  const sds name = syntax->identifier_token.text;
  variable_symbol_t* variable = bound_scope_try_lookup(binder->scope, name);
  if (variable == NULL) {
    diagnostic_bag_report_undefined_variable(
        &binder->diagnostics,
        token_span(&syntax->identifier_token),
        name
    );
    return expression;
  }
  if (bound_expression_type(expression) != variable->type) {
    diagnostic_bag_report_cannot_convert(
        &binder->diagnostics,
        syntax_node_span((syntax_node_t*)syntax->expression),
        bound_expression_type(expression),
        variable->type
    );
    return expression;
  }
  bound_expression_t* result = bound_assignment_expression_new(
      variable_symbol_copy(variable),
      expression
  );
  return result;
}

static bound_expression_t*
bind_name_expression(binder_t* binder, const name_expression_syntax_t* syntax) {
  (void)binder;
  variable_symbol_t* variable =
      bound_scope_try_lookup(binder->scope, syntax->identifier_token.text);
  if (variable == NULL) {
    diagnostic_bag_report_undefined_variable(
        &binder->diagnostics,
        token_span(&syntax->identifier_token),
        syntax->identifier_token.text
    );
    return bound_literal_expression_new(integer_new(0));
  }
  return bound_variable_expression_new(variable_symbol_copy(variable));
}

static bound_expression_t* bind_parenthesized_expression(
    binder_t* binder,
    const parenthesized_expression_syntax_t* syntax
) {
  bound_expression_t* expression =
      binder_bind_expression(binder, syntax->expression);
  return expression;
}

static bound_expression_t* bind_unary_expression(
    binder_t* binder,
    const unary_expression_syntax_t* syntax
) {
  bound_expression_t* operand = binder_bind_expression(binder, syntax->operand);
  const bound_unary_operator_t* op = bound_unary_operator_bind(
      syntax->operator_token.base.kind,
      bound_expression_type(operand)
  );
  if (op == NULL) {
    diagnostic_bag_report_undefined_unary_operator(
        &binder->diagnostics,
        token_span(&syntax->operator_token),
        syntax->operator_token.text,
        bound_expression_type(operand)
    );
  }
  bound_expression_t* expression = bound_unary_expression_new(op, operand);
  return expression;
}

bound_expression_t* binder_bind_expression(
    binder_t* binder,
    const expression_syntax_t* expression
) {
  switch (expression->base.kind) {
  case syntax_kind_assignment_expression:
    return bind_assignment_expression(
        binder,
        (const assignment_expression_syntax_t*)expression
    );
  case syntax_kind_binary_expression:
    return bind_binary_expression(
        binder,
        (const binary_expression_syntax_t*)expression
    );
  case syntax_kind_literal_expression:
    return bind_literal_expression(
        binder,
        (const literal_expression_syntax_t*)expression
    );
  case syntax_kind_name_expression:
    return bind_name_expression(
        binder,
        (const name_expression_syntax_t*)expression
    );
  case syntax_kind_parenthesized_expression:
    return bind_parenthesized_expression(
        binder,
        (const parenthesized_expression_syntax_t*)expression
    );
  case syntax_kind_unary_expression:
    return bind_unary_expression(
        binder,
        (const unary_expression_syntax_t*)expression
    );
  default:
    assert(false && "corrupt syntax node");
  }
}

void binder_free(binder_t* binder) {
  // diagnostic_bag_free(&binder->diagnostics);
  bound_scope_free(binder->scope);
}
