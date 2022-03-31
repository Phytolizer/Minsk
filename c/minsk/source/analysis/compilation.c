#include "minsk/analysis/compilation.h"
#include "minsk/analysis/binding/binder.h"
#include "minsk/analysis/binding/node.h"
#include "minsk/analysis/binding/node/expression.h"
#include "minsk/analysis/binding/scope/global.h"
#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/evaluator.h"
#include "minsk/analysis/variables.h"
#include "minsk/runtime/object.h"
#include <stdlib.h>

void compilation_init(compilation_t* compilation, compilation_t* previous,
    const syntax_tree_t* syntax) {
  compilation->previous = previous;
  compilation->syntax = syntax;
  bound_global_scope_t* previous_global_scope = NULL;
  if (previous != NULL) {
    previous_global_scope = &previous->global_scope;
  }
  compilation->global_scope =
      binder_bind_global_scope(previous_global_scope, &syntax->root);
}

compilation_t compilation_continue_with(
    compilation_t* compilation, const syntax_tree_t* syntax) {
  compilation_t result;
  compilation_init(&result, compilation, syntax);
  return result;
}

evaluation_result_t compilation_evaluate(
    compilation_t* compilation, variable_map_t* variables) {
  diagnostic_bag_t diagnostics;
  diagnostic_bag_init(&diagnostics);
  for (size_t i = 0; i < compilation->syntax->diagnostics.length; i++) {
    diagnostic_bag_copy_diagnostic(
        &diagnostics, compilation->syntax->diagnostics.data[i]);
  }
  for (size_t i = 0; i < compilation->global_scope.diagnostics.length; i++) {
    diagnostic_bag_copy_diagnostic(
        &diagnostics, compilation->global_scope.diagnostics.data[i]);
  }
  if (diagnostics.length > 0) {
    return (evaluation_result_t){.diagnostics = diagnostics};
  }
  evaluator_t evaluator;
  evaluator_init(&evaluator, compilation->global_scope.expression, variables);
  object_t* result = evaluator_evaluate(&evaluator);
  return (evaluation_result_t){.value = result};
}

void compilation_free(compilation_t* compilation) {
  if (compilation->previous != NULL) {
    compilation_free(compilation->previous);
    free(compilation->previous);
  }
  bound_global_scope_free(&compilation->global_scope);
}
