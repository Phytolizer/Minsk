#include "minsk/analysis/binding/nodes/statements/expression.hpp"
#include "minsk/analysis/binding/kind.hpp"

minsk::analysis::binding::bound_expression_statement::
    bound_expression_statement(std::unique_ptr<bound_expression> expression)
    : m_expression(std::move(expression)) {}

minsk::analysis::binding::bound_node_kind
minsk::analysis::binding::bound_expression_statement::kind() const {
  return bound_node_kind::expression_statement;
}

const minsk::analysis::binding::bound_expression *
minsk::analysis::binding::bound_expression_statement::expression() const {
  return m_expression.get();
}
