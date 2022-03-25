#include "minsk/analysis/syntax/nodes/expressions/assignment.hpp"
#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/node.hpp"
#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include <vector>

minsk::analysis::syntax::assignment_expression_syntax::
    assignment_expression_syntax(syntax_token &&identifier_token,
                                 syntax_token &&equals_token,
                                 std::unique_ptr<expression_syntax> expression)
    : m_identifier_token(std::move(identifier_token)),
      m_equals_token(std::move(equals_token)),
      m_expression(std::move(expression)) {}

minsk::analysis::syntax::syntax_kind
minsk::analysis::syntax::assignment_expression_syntax::kind() const {
  return syntax_kind::assignment_expression;
}

std::vector<const minsk::analysis::syntax::node *>
minsk::analysis::syntax::assignment_expression_syntax::children() const {
  return {
      &m_identifier_token,
      &m_equals_token,
      m_expression.get(),
  };
}

const minsk::analysis::syntax::syntax_token &
minsk::analysis::syntax::assignment_expression_syntax::identifier_token()
    const {
  return m_identifier_token;
}

const minsk::analysis::syntax::syntax_token &
minsk::analysis::syntax::assignment_expression_syntax::equals_token() const {
  return m_equals_token;
}

const minsk::analysis::syntax::expression_syntax *
minsk::analysis::syntax::assignment_expression_syntax::expression() const {
  return m_expression.get();
}
