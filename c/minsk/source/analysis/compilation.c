#include "minsk/analysis/compilation.h"
#include "minsk/analysis/binding/binder.h"
#include "minsk/analysis/binding/node.h"
#include "minsk/analysis/binding/node/expression.h"
#include "minsk/analysis/diagnostic_bag.h"
#include "minsk/analysis/evaluator.h"
#include "minsk/runtime/object.h"

void compilation_init(compilation_t *compilation, const syntax_tree_t *syntax) {
  compilation->syntax = syntax;
}

evaluation_result_t compilation_evaluate(compilation_t *compilation) {
  binder_t binder;
  binder_init(&binder);
  bound_expression_t *bound_expression =
      binder_bind_expression(&binder, compilation->syntax->root);
  diagnostic_bag_t diagnostics;
  diagnostic_bag_init(&diagnostics);
  for (size_t i = 0; i < compilation->syntax->diagnostics.length; i++) {
    diagnostic_bag_copy_diagnostic(&diagnostics,
                                   compilation->syntax->diagnostics.data[i]);
  }
  for (size_t i = 0; i < binder.diagnostics.length; i++) {
    diagnostic_bag_copy_diagnostic(&diagnostics, binder.diagnostics.data[i]);
  }
  binder_free(&binder);
  if (diagnostics.length > 0) {
    bound_node_free((bound_node_t *)bound_expression);
    return (evaluation_result_t){.diagnostics = diagnostics};
  }
  evaluator_t evaluator;
  evaluator_init(&evaluator, bound_expression);
  object_t *result = evaluator_evaluate(&evaluator);
  bound_node_free((bound_node_t *)bound_expression);
  return (evaluation_result_t){.value = result};
}
