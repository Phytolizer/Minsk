#include "minsk/analysis/binding/nodes/expressions/variable.hpp"
#include "minsk/analysis/binding/kind.hpp"
#include "minsk/analysis/variable_symbol.hpp"
#include "minsk/runtime/object.hpp"

minsk::analysis::binding::bound_variable_expression::bound_variable_expression(
    variable_symbol &&variable)
    : m_variable(std::move(variable)) {}

minsk::analysis::binding::bound_node_kind
minsk::analysis::binding::bound_variable_expression::kind() const {
  return bound_node_kind::variable_expression;
}

minsk::runtime::object_kind
minsk::analysis::binding::bound_variable_expression::type() const {
  return m_variable.type();
}

const minsk::analysis::variable_symbol &
minsk::analysis::binding::bound_variable_expression::variable() const {
  return m_variable;
}
