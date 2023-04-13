#include "minsk/code_analysis/compilation.h"

#include "minsk/code_analysis/binding/binder.h"
#include "minsk/code_analysis/evaluator.h"

extern minsk_compilation_t
minsk_compilation_new(Arena * arena, minsk_syntax_tree_t syntax_tree)
{
  return (minsk_compilation_t){
    ._arena = arena,
    .syntax_tree = syntax_tree,
  };
}

extern minsk_evaluation_result_t
minsk_compilation_evaluate(minsk_compilation_t * compilation)
{
  minsk_binder_t binder = minsk_binder_new(compilation->_arena);
  minsk_bound_node_t bound_expression =
    minsk_binder_bind_expression(&binder, compilation->syntax_tree.root);

  minsk_diagnostic_buf_t diagnostics = compilation->syntax_tree.diagnostics;
  BUF_APPEND_ARENA(compilation->_arena, &diagnostics, binder.diagnostics);
  if (diagnostics.len > 0)
  {
    return (minsk_evaluation_result_t){
      .success = false,
      .diagnostics = diagnostics,
    };
  }

  minsk_evaluator_t evaluator = minsk_evaluator_new(bound_expression);
  minsk_object_t value = minsk_evaluator_evaluate(&evaluator);
  return (minsk_evaluation_result_t){
    .success = true,
    .value = value,
  };
}
