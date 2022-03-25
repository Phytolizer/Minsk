#include "minsk/analysis/syntax/nodes/unit.hpp"
#include "minsk/analysis/syntax/kind.hpp"

minsk::analysis::syntax::compilation_unit_syntax::compilation_unit_syntax(
    std::unique_ptr<expression_syntax> expression,
    syntax_token &&end_of_file_token)
    : m_expression(std::move(expression)),
      m_end_of_file_token(std::move(end_of_file_token)) {}

minsk::analysis::syntax::syntax_kind
minsk::analysis::syntax::compilation_unit_syntax::kind() const {
  return syntax_kind::compilation_unit;
}

std::vector<const minsk::analysis::syntax::syntax_node *>
minsk::analysis::syntax::compilation_unit_syntax::children() const {
  return {
      m_expression.get(),
      &m_end_of_file_token,
  };
}

const minsk::analysis::syntax::expression_syntax *
minsk::analysis::syntax::compilation_unit_syntax::expression() const {
  return m_expression.get();
}

const minsk::analysis::syntax::syntax_token &
minsk::analysis::syntax::compilation_unit_syntax::end_of_file_token() const {
  return m_end_of_file_token;
}
