#pragma once

#include <arena.h>

#include "minsk/code_analysis/binding/ast/node.h"
#include "minsk/code_analysis/diagnostic_bag.h"
#include "minsk/code_analysis/syntax/ast/node.h"
#include "minsk/code_analysis/variable_map.h"

typedef struct
{
  Arena * _arena;
  minsk_variable_map_t * _variables;
  minsk_diagnostic_bag_t diagnostics;
} minsk_binder_t;

extern minsk_binder_t
minsk_binder_new(Arena * arena, minsk_variable_map_t * variables);
extern minsk_bound_node_t
minsk_binder_bind_expression(
  minsk_binder_t * binder,
  minsk_syntax_node_t syntax
);
