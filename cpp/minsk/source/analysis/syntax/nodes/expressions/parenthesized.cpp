#include "minsk/analysis/syntax/nodes/expressions/parenthesized.hpp"
#include <utility>

using minsk::analysis::syntax::expression_syntax;
using minsk::analysis::syntax::parenthesized_expression_syntax;
using minsk::analysis::syntax::syntax_kind;
using minsk::analysis::syntax::syntax_node;
using minsk::analysis::syntax::syntax_token;

parenthesized_expression_syntax::parenthesized_expression_syntax(
    syntax_token &&open_parenthesis_token,
    std::unique_ptr<expression_syntax> expression,
    syntax_token &&close_parenthesis_token)
    : m_open_parenthesis_token(std::move(open_parenthesis_token)),
      m_expression(std::move(expression)),
      m_close_parenthesis_token(std::move(close_parenthesis_token)) {}

syntax_kind parenthesized_expression_syntax::kind() const {
  return syntax_kind::parenthesized_expression;
}

std::vector<const syntax_node *>
parenthesized_expression_syntax::children() const {
  return {
      &m_open_parenthesis_token,
      m_expression.get(),
      &m_close_parenthesis_token,
  };
}

const syntax_token &
parenthesized_expression_syntax::open_parenthesis_token() const {
  return m_open_parenthesis_token;
}

const expression_syntax *parenthesized_expression_syntax::expression() const {
  return m_expression.get();
}

const syntax_token &
parenthesized_expression_syntax::close_parenthesis_token() const {
  return m_close_parenthesis_token;
}
