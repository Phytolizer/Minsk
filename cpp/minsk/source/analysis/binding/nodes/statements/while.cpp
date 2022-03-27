#include "minsk/analysis/binding/nodes/statements/while.hpp"

minsk::analysis::binding::bound_while_statement::bound_while_statement(
    std::unique_ptr<bound_expression> condition,
    std::unique_ptr<bound_statement> body)
    : m_condition(std::move(condition)), m_body(std::move(body)) {}

minsk::analysis::binding::bound_node_kind
minsk::analysis::binding::bound_while_statement::kind() const {
  return bound_node_kind::while_statement;
}

const minsk::analysis::binding::bound_expression *
minsk::analysis::binding::bound_while_statement::condition() const {
  return m_condition.get();
}

const minsk::analysis::binding::bound_statement *
minsk::analysis::binding::bound_while_statement::body() const {
  return m_body.get();
}
