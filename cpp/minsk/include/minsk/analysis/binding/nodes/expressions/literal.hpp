#ifndef MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_LITERAL_HPP
#define MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_LITERAL_HPP

#include "minsk/analysis/binding/kind.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/runtime/object.hpp"

namespace minsk::analysis::binding {

class bound_literal_expression final : public bound_expression {
  runtime::object_ptr m_value;

public:
  explicit bound_literal_expression(runtime::object_ptr value);
  bound_node_kind kind() const override;
  runtime::object_kind type() const override;
  const runtime::object *value() const;
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_LITERAL_HPP
