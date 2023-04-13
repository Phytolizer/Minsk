#pragma once

#include "minsk/code_analysis/binding/ast/node.h"
#include "minsk/code_analysis/variable_map.h"
#include "minsk/runtime/object.h"

typedef struct
{
  minsk_bound_node_t _root;
  minsk_variable_map_t * _variables;
} minsk_evaluator_t;

extern minsk_evaluator_t
minsk_evaluator_new(minsk_bound_node_t root, minsk_variable_map_t * variables);
extern minsk_object_t
minsk_evaluator_evaluate(minsk_evaluator_t * evaluator);
