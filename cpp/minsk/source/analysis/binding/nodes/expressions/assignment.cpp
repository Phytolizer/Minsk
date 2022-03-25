#include "minsk/analysis/binding/nodes/expressions/assignment.hpp"
#include "minsk/analysis/binding/kind.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/variable_symbol.hpp"
#include "minsk/runtime/object.hpp"

minsk::analysis::binding::bound_assignment_expression::
    bound_assignment_expression(variable_symbol &&variable,
                                std::unique_ptr<bound_expression> expression)
    : m_variable(std::move(variable)), m_expression(std::move(expression)) {}

minsk::analysis::binding::bound_node_kind
minsk::analysis::binding::bound_assignment_expression::kind() const {
  return bound_node_kind::assignment_expression;
}

minsk::runtime::object_kind
minsk::analysis::binding::bound_assignment_expression::type() const {
  return m_variable.type();
}

const minsk::analysis::variable_symbol &
minsk::analysis::binding::bound_assignment_expression::variable() const {
  return m_variable;
}

const minsk::analysis::binding::bound_expression *
minsk::analysis::binding::bound_assignment_expression::expression() const {
  return m_expression.get();
}
