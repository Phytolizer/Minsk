#include "minsk/analysis/binding/nodes/expressions/assignment.hpp"
#include "minsk/analysis/binding/kind.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/runtime/object.hpp"

minsk::analysis::binding::bound_assignment_expression::
    bound_assignment_expression(std::string &&name,
                                std::unique_ptr<bound_expression> expression)
    : m_name(std::move(name)), m_expression(std::move(expression)) {}

minsk::analysis::binding::bound_node_kind
minsk::analysis::binding::bound_assignment_expression::kind() const {
  return bound_node_kind::assignment_expression;
}

minsk::runtime::object_kind
minsk::analysis::binding::bound_assignment_expression::type() const {
  return m_expression->type();
}

std::string_view
minsk::analysis::binding::bound_assignment_expression::name() const {
  return m_name;
}

const minsk::analysis::binding::bound_expression *
minsk::analysis::binding::bound_assignment_expression::expression() const {
  return m_expression.get();
}
