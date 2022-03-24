#include "minsk/analysis/syntax/nodes/expressions/unary.hpp"
minsk::analysis::syntax::unary_expression_syntax::unary_expression_syntax(
    minsk::analysis::syntax::syntax_token &&operator_token,
    std::unique_ptr<expression_syntax> operand)
    : m_operator_token(std::move(operator_token)),
      m_operand(std::move(operand)) {}
minsk::analysis::syntax::syntax_kind
minsk::analysis::syntax::unary_expression_syntax::kind() const {
  return syntax_kind::unary_expression;
}
std::vector<const minsk::analysis::syntax::node *>
minsk::analysis::syntax::unary_expression_syntax::children() const {
  return {
      &m_operator_token,
      m_operand.get(),
  };
}
const minsk::analysis::syntax::syntax_token &
minsk::analysis::syntax::unary_expression_syntax::operator_token() const {
  return m_operator_token;
}
const minsk::analysis::syntax::expression_syntax *
minsk::analysis::syntax::unary_expression_syntax::operand() const {
  return m_operand.get();
}
