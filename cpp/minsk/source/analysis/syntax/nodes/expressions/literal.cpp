#include "minsk/analysis/syntax/nodes/expressions/literal.hpp"
minsk::analysis::syntax::literal_expression_syntax::literal_expression_syntax(
    minsk::analysis::syntax::syntax_token &&literal_token)
    : m_literal_token(std::move(literal_token)) {}
minsk::analysis::syntax::syntax_kind
minsk::analysis::syntax::literal_expression_syntax::kind() const {
  return syntax_kind::literal_expression;
}
std::vector<const minsk::analysis::syntax::node *>
minsk::analysis::syntax::literal_expression_syntax::children() const {
  return {
      &m_literal_token,
  };
}
const minsk::analysis::syntax::syntax_token &
minsk::analysis::syntax::literal_expression_syntax::literal_token() const {
  return m_literal_token;
}
