#include "minsk/analysis/binding/nodes/expressions/literal.hpp"
#include "minsk/analysis/binding/kind.hpp"
#include "minsk/runtime/object.hpp"

minsk::analysis::binding::bound_literal_expression::bound_literal_expression(
    runtime::object_ptr value)
    : m_value(std::move(value)) {}
minsk::analysis::binding::bound_node_kind
minsk::analysis::binding::bound_literal_expression::kind() const {
  return bound_node_kind::literal_expression;
}
minsk::runtime::object_kind
minsk::analysis::binding::bound_literal_expression::type() const {
  return m_value->kind();
}
const minsk::runtime::object *
minsk::analysis::binding::bound_literal_expression::value() const {
  return m_value.get();
}
