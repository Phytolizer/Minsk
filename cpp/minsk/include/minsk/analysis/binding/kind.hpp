#ifndef MINSK_ANALYSIS_BINDING_KIND_HPP
#define MINSK_ANALYSIS_BINDING_KIND_HPP

namespace minsk::analysis::binding {

enum class bound_node_kind {
  binary_expression,
  literal_expression,
  unary_expression,
};

}

#endif // MINSK_ANALYSIS_BINDING_KIND_HPP
