#include "minsk/analysis/syntax/nodes/expressions/unary.hpp"
#include <utility>

using minsk::analysis::syntax::expression_syntax;
using minsk::analysis::syntax::syntax_kind;
using minsk::analysis::syntax::syntax_node;
using minsk::analysis::syntax::syntax_token;
using minsk::analysis::syntax::unary_expression_syntax;

unary_expression_syntax::unary_expression_syntax(
    syntax_token &&operator_token, std::unique_ptr<expression_syntax> operand)
    : m_operator_token(std::move(operator_token)),
      m_operand(std::move(operand)) {}

syntax_kind unary_expression_syntax::kind() const {
  return syntax_kind::unary_expression;
}

std::vector<const syntax_node *> unary_expression_syntax::children() const {
  return {
      &m_operator_token,
      m_operand.get(),
  };
}

const syntax_token &unary_expression_syntax::operator_token() const {
  return m_operator_token;
}

const expression_syntax *unary_expression_syntax::operand() const {
  return m_operand.get();
}
