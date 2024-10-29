#include "minsk/analysis/syntax/nodes/statements/variable.hpp"

using minsk::analysis::syntax::expression_syntax;
using minsk::analysis::syntax::syntax_kind;
using minsk::analysis::syntax::syntax_node;
using minsk::analysis::syntax::syntax_token;
using minsk::analysis::syntax::variable_declaration_syntax;

variable_declaration_syntax::variable_declaration_syntax(
    syntax_token &&keyword_token, syntax_token &&identifier_token,
    syntax_token &&equals_token, std::unique_ptr<expression_syntax> initializer)
    : m_keyword_token(std::move(keyword_token)),
      m_identifier_token(std::move(identifier_token)),
      m_equals_token(std::move(equals_token)),
      m_initializer(std::move(initializer)) {}

syntax_kind variable_declaration_syntax::kind() const {
  return syntax_kind::variable_declaration;
}

std::vector<const syntax_node *> variable_declaration_syntax::children() const {
  return {
      &m_keyword_token,
      &m_identifier_token,
      &m_equals_token,
      m_initializer.get(),
  };
}

const syntax_token &variable_declaration_syntax::keyword_token() const {
  return m_keyword_token;
}

const syntax_token &variable_declaration_syntax::identifier_token() const {
  return m_identifier_token;
}

const syntax_token &variable_declaration_syntax::equals_token() const {
  return m_equals_token;
}

const expression_syntax *variable_declaration_syntax::initializer() const {
  return m_initializer.get();
}
