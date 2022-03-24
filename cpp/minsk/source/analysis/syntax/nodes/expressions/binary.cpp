#include "minsk/analysis/syntax/nodes/expressions/binary.hpp"
#include <utility>
minsk::analysis::syntax::binary_expression_syntax::binary_expression_syntax(
    std::unique_ptr<expression_syntax> left,
    minsk::analysis::syntax::syntax_token &&operator_token,
    std::unique_ptr<expression_syntax> right)
    : m_left(std::move(left)), m_operator_token(std::move(operator_token)),
      m_right(std::move(right)) {}
minsk::analysis::syntax::syntax_kind
minsk::analysis::syntax::binary_expression_syntax::kind() const {
  return syntax_kind::binary_expression;
}
std::vector<const minsk::analysis::syntax::node *>
minsk::analysis::syntax::binary_expression_syntax::children() const {
  return {
      m_left.get(),
      &m_operator_token,
      m_right.get(),
  };
}
const minsk::analysis::syntax::expression_syntax *
minsk::analysis::syntax::binary_expression_syntax::left() const {
  return m_left.get();
}
const minsk::analysis::syntax::syntax_token &
minsk::analysis::syntax::binary_expression_syntax::operator_token() const {
  return m_operator_token;
}
const minsk::analysis::syntax::expression_syntax *
minsk::analysis::syntax::binary_expression_syntax::right() const {
  return m_right.get();
}
