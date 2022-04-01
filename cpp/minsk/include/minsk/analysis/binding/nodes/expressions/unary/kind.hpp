#ifndef MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_UNARY_KIND_HPP
#define MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_UNARY_KIND_HPP

namespace minsk::analysis::binding {

enum class bound_unary_operator_kind {
  identity,
  negation,
  logical_negation,
  bitwise_negation,
};

}

#endif // MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_UNARY_KIND_HPP
