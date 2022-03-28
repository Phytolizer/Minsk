#include "minsk/analysis/compilation.h"
#include "minsk/analysis/binding/binder.h"
#include "minsk/analysis/binding/node.h"
#include "minsk/analysis/binding/node/expression.h"
#include "minsk/analysis/binding/scope/global.h"
#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/evaluator.h"
#include "minsk/analysis/variables.h"
#include "minsk/runtime/object.h"

void compilation_init(compilation_t *compilation, const syntax_tree_t *syntax) {
  compilation->syntax = syntax;
}

evaluation_result_t compilation_evaluate(compilation_t *compilation,
                                         variable_map_t *variables) {
  bound_global_scope_t global_scope =
      binder_bind_global_scope(&compilation->syntax->root);
  diagnostic_bag_t diagnostics;
  diagnostic_bag_init(&diagnostics);
  for (size_t i = 0; i < compilation->syntax->diagnostics.length; i++) {
    diagnostic_bag_copy_diagnostic(&diagnostics,
                                   compilation->syntax->diagnostics.data[i]);
  }
  for (size_t i = 0; i < global_scope.diagnostics.length; i++) {
    diagnostic_bag_copy_diagnostic(&diagnostics,
                                   global_scope.diagnostics.data[i]);
  }
  if (diagnostics.length > 0) {
    bound_global_scope_free(&global_scope);
    return (evaluation_result_t){.diagnostics = diagnostics};
  }
  evaluator_t evaluator;
  evaluator_init(&evaluator, global_scope.expression, variables);
  object_t *result = evaluator_evaluate(&evaluator);
  bound_global_scope_free(&global_scope);
  return (evaluation_result_t){.value = result};
}
