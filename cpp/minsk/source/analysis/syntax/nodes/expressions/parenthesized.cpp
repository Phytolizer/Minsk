#include "minsk/analysis/syntax/nodes/expressions/parenthesized.hpp"
#include <utility>

minsk::analysis::syntax::parenthesized_expression_syntax::
    parenthesized_expression_syntax(
        minsk::analysis::syntax::syntax_token &&open_parenthesis_token,
        std::unique_ptr<expression_syntax> expression,
        minsk::analysis::syntax::syntax_token &&close_parenthesis_token)
    : m_open_parenthesis_token(std::move(open_parenthesis_token)),
      m_expression(std::move(expression)),
      m_close_parenthesis_token(std::move(close_parenthesis_token)) {}
minsk::analysis::syntax::syntax_kind
minsk::analysis::syntax::parenthesized_expression_syntax::kind() const {
  return syntax_kind::parenthesized_expression;
}
std::vector<const minsk::analysis::syntax::syntax_node *>
minsk::analysis::syntax::parenthesized_expression_syntax::children() const {
  return {
      &m_open_parenthesis_token,
      m_expression.get(),
      &m_close_parenthesis_token,
  };
}
const minsk::analysis::syntax::syntax_token &minsk::analysis::syntax::
    parenthesized_expression_syntax::open_parenthesis_token() const {
  return m_open_parenthesis_token;
}
const minsk::analysis::syntax::expression_syntax *
minsk::analysis::syntax::parenthesized_expression_syntax::expression() const {
  return m_expression.get();
}
const minsk::analysis::syntax::syntax_token &minsk::analysis::syntax::
    parenthesized_expression_syntax::close_parenthesis_token() const {
  return m_close_parenthesis_token;
}
