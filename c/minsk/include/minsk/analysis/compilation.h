#pragma once

#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/syntax/tree.h"
#include "minsk/analysis/variables.h"
#include "minsk/runtime/object.h"

typedef struct {
  const syntax_tree_t *syntax;
} compilation_t;

typedef struct {
  diagnostic_bag_t diagnostics;
  object_t *value;
} evaluation_result_t;

void compilation_init(compilation_t *compilation, const syntax_tree_t *syntax);
evaluation_result_t compilation_evaluate(compilation_t *compilation,
                                         variable_map_t *variables);
