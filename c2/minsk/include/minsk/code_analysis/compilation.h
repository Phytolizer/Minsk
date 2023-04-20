#pragma once

#include <arena.h>
#include <stdbool.h>

#include "minsk/code_analysis/diagnostic_bag.h"
#include "minsk/code_analysis/syntax/tree.h"
#include "minsk/code_analysis/variable_map.h"
#include "minsk/runtime/object.h"

typedef struct
{
  Arena * _arena;
  minsk_syntax_tree_t syntax_tree;
} minsk_compilation_t;

typedef struct
{
  bool success;

  union
  {
    minsk_diagnostic_bag_buf_t diagnostics;
    minsk_object_t value;
  };
} minsk_evaluation_result_t;

extern minsk_compilation_t
minsk_compilation_new(Arena * arena, minsk_syntax_tree_t syntax_tree);
extern minsk_evaluation_result_t
minsk_compilation_evaluate(
  minsk_compilation_t * compilation,
  minsk_variable_map_t * variables
);
