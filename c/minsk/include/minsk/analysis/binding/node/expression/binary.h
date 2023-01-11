#pragma once

#include "minsk/analysis/binding/node/expression.h"
#include "minsk/analysis/binding/node/expression/binary/operator.h"
#include "minsk/runtime/object.h"

typedef struct {
  bound_expression_t base;
  bound_expression_t* left;
  const bound_binary_operator_t* op;
  bound_expression_t* right;
} bound_binary_expression_t;

bound_expression_t* bound_binary_expression_new(
    bound_expression_t* left,
    const bound_binary_operator_t* op,
    bound_expression_t* right
);
object_kind_t
bound_binary_expression_type(const bound_binary_expression_t* expression);
void bound_binary_expression_free(bound_binary_expression_t* expression);
