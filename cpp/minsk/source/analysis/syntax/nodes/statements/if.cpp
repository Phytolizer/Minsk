#include "minsk/analysis/syntax/nodes/statements/if.hpp"
#include "minsk/analysis/syntax/nodes/else.hpp"

using minsk::analysis::syntax::else_clause_syntax;
using minsk::analysis::syntax::expression_syntax;
using minsk::analysis::syntax::if_statement_syntax;
using minsk::analysis::syntax::statement_syntax;
using minsk::analysis::syntax::syntax_kind;
using minsk::analysis::syntax::syntax_node;
using minsk::analysis::syntax::syntax_token;

if_statement_syntax::if_statement_syntax(
    syntax_token &&keyword_token, std::unique_ptr<expression_syntax> condition,
    std::unique_ptr<statement_syntax> then_statement,
    std::unique_ptr<else_clause_syntax> else_clause)
    : m_keyword_token(std::move(keyword_token)),
      m_condition(std::move(condition)),
      m_then_statement(std::move(then_statement)),
      m_else_clause(std::move(else_clause)) {}

syntax_kind if_statement_syntax::kind() const {
  return syntax_kind::if_statement;
}

std::vector<const syntax_node *> if_statement_syntax::children() const {
  if (m_else_clause) {
    return {
        &m_keyword_token,
        m_condition.get(),
        m_then_statement.get(),
        m_else_clause.get(),
    };
  }
  return {
      &m_keyword_token,
      m_condition.get(),
      m_then_statement.get(),
  };
}

const syntax_token &if_statement_syntax::keyword_token() const {
  return m_keyword_token;
}

const expression_syntax *if_statement_syntax::condition() const {
  return m_condition.get();
}

const statement_syntax *if_statement_syntax::then_statement() const {
  return m_then_statement.get();
}

const else_clause_syntax *if_statement_syntax::else_clause() const {
  return m_else_clause.get();
}
