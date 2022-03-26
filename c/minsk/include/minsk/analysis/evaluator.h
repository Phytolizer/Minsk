#pragma once

#include "minsk/analysis/binding/node/expression.h"
#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/runtime/object.h"

typedef struct {
  const bound_expression_t *root;
} evaluator_t;

void evaluator_init(evaluator_t *evaluator, const bound_expression_t *root);
object_t *evaluator_evaluate(evaluator_t *evaluator);
