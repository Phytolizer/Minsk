#include "minsk/analysis/binding/nodes/expressions/variable.hpp"
#include "minsk/analysis/binding/kind.hpp"
#include "minsk/runtime/object.hpp"
minsk::analysis::binding::bound_variable_expression::bound_variable_expression(
    std::string &&name, runtime::object_kind type)
    : m_name(std::move(name)), m_type(type) {}

minsk::analysis::binding::bound_node_kind
minsk::analysis::binding::bound_variable_expression::kind() const {
  return bound_node_kind::variable_expression;
}

minsk::runtime::object_kind
minsk::analysis::binding::bound_variable_expression::type() const {
  return m_type;
}

std::string_view minsk::analysis::binding::bound_variable_expression::name() const {
  return m_name;
}
