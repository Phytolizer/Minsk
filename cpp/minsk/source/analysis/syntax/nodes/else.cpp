#include "minsk/analysis/syntax/nodes/else.hpp"

using minsk::analysis::syntax::else_clause_syntax;
using minsk::analysis::syntax::statement_syntax;
using minsk::analysis::syntax::syntax_kind;
using minsk::analysis::syntax::syntax_node;
using minsk::analysis::syntax::syntax_token;

else_clause_syntax::else_clause_syntax(
    syntax_token &&keyword_token,
    std::unique_ptr<statement_syntax> else_statement)
    : m_keyword_token(std::move(keyword_token)),
      m_else_statement(std::move(else_statement)) {}

syntax_kind else_clause_syntax::kind() const {
  return syntax_kind::else_clause;
}

std::vector<const syntax_node *> else_clause_syntax::children() const {
  return {
      &m_keyword_token,
      m_else_statement.get(),
  };
}

const syntax_token &else_clause_syntax::keyword_token() const {
  return m_keyword_token;
}

const statement_syntax *else_clause_syntax::else_statement() const {
  return m_else_statement.get();
}
