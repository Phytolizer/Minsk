#pragma once

#include "minsk/analysis/syntax/node/expression.h"
#include "minsk/runtime/object.h"

typedef struct {
  expression_syntax_t *root;
} evaluator_t;

void evaluator_init(evaluator_t *evaluator, expression_syntax_t *root);
object_t *evaluator_evaluate(evaluator_t *evaluator);
