#ifndef MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_BINARY_KIND_HPP
#define MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_BINARY_KIND_HPP

namespace minsk::analysis::binding {

enum class bound_binary_operator_kind {
  addition,
  subtraction,
  multiplication,
  division,
  logical_and,
  logical_or,
  equality,
  inequality,
  less_than,
  less_than_or_equal,
  greater_than,
  greater_than_or_equal,
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_BINARY_KIND_HPP
