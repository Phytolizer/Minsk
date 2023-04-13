#pragma once

#include "./syntax/ast/node.h"
#include "minsk/runtime/object.h"

typedef struct
{
  minsk_syntax_node_t _root;
} minsk_evaluator_t;

extern minsk_evaluator_t minsk_evaluator_new(minsk_syntax_node_t root);
extern minsk_object_t minsk_evaluator_evaluate(minsk_evaluator_t* evaluator);
