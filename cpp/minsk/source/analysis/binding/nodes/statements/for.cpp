#include "minsk/analysis/binding/nodes/statements/for.hpp"

minsk::analysis::binding::bound_for_statement::bound_for_statement(
    variable_symbol &&variable, std::unique_ptr<bound_expression> initial_value,
    std::unique_ptr<bound_expression> final_value,
    std::unique_ptr<bound_statement> body)
    : m_variable(std::move(variable)),
      m_initial_value(std::move(initial_value)),
      m_final_value(std::move(final_value)), m_body(std::move(body)) {}

minsk::analysis::binding::bound_node_kind
minsk::analysis::binding::bound_for_statement::kind() const {
  return bound_node_kind::for_statement;
}

const minsk::analysis::variable_symbol &
minsk::analysis::binding::bound_for_statement::variable() const {
  return m_variable;
}

const minsk::analysis::binding::bound_expression *
minsk::analysis::binding::bound_for_statement::initial_value() const {
  return m_initial_value.get();
}

const minsk::analysis::binding::bound_expression *
minsk::analysis::binding::bound_for_statement::final_value() const {
  return m_final_value.get();
}

const minsk::analysis::binding::bound_statement *
minsk::analysis::binding::bound_for_statement::body() const {
  return m_body.get();
}
