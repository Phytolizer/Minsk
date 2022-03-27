#include "minsk/analysis/binding/nodes/statements/if.hpp"

minsk::analysis::binding::bound_if_statement::bound_if_statement(
    std::unique_ptr<bound_expression> condition,
    std::unique_ptr<bound_statement> then_statement,
    std::unique_ptr<bound_statement> else_statement)
    : m_condition(std::move(condition)),
      m_then_statement(std::move(then_statement)),
      m_else_statement(std::move(else_statement)) {}

minsk::analysis::binding::bound_node_kind
minsk::analysis::binding::bound_if_statement::kind() const {
  return bound_node_kind::if_statement;
}

const minsk::analysis::binding::bound_expression *
minsk::analysis::binding::bound_if_statement::condition() const {
  return m_condition.get();
}

const minsk::analysis::binding::bound_statement *
minsk::analysis::binding::bound_if_statement::then_statement() const {
  return m_then_statement.get();
}

const minsk::analysis::binding::bound_statement *
minsk::analysis::binding::bound_if_statement::else_statement() const {
  return m_else_statement.get();
}
