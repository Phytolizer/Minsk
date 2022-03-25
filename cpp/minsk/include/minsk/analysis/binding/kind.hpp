#ifndef MINSK_ANALYSIS_BINDING_KIND_HPP
#define MINSK_ANALYSIS_BINDING_KIND_HPP

namespace minsk::analysis::binding {

enum class bound_node_kind {
  block_statement,
  expression_statement,
  variable_declaration,

  assignment_expression,
  binary_expression,
  literal_expression,
  unary_expression,
  variable_expression,
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_KIND_HPP
