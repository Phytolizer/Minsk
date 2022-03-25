#include "minsk/analysis/syntax/nodes/expressions/name.hpp"
#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/node.hpp"
#include "minsk/analysis/syntax/token.hpp"

minsk::analysis::syntax::name_expression_syntax::name_expression_syntax(
    syntax_token &&identifier_token)
    : m_identifier_token(std::move(identifier_token)) {}

minsk::analysis::syntax::syntax_kind
minsk::analysis::syntax::name_expression_syntax::kind() const {
  return syntax_kind::name_expression;
}

std::vector<const minsk::analysis::syntax::syntax_node *>
minsk::analysis::syntax::name_expression_syntax::children() const {
  return {
      &m_identifier_token,
  };
}

const minsk::analysis::syntax::syntax_token &
minsk::analysis::syntax::name_expression_syntax::identifier_token() const {
  return m_identifier_token;
}
