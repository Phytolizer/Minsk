#include "minsk/analysis/syntax/nodes/statements/while.hpp"

using minsk::analysis::syntax::expression_syntax;
using minsk::analysis::syntax::statement_syntax;
using minsk::analysis::syntax::syntax_kind;
using minsk::analysis::syntax::syntax_node;
using minsk::analysis::syntax::syntax_token;
using minsk::analysis::syntax::while_statement_syntax;

while_statement_syntax::while_statement_syntax(
    syntax_token &&while_keyword, std::unique_ptr<expression_syntax> condition,
    std::unique_ptr<statement_syntax> body)
    : m_while_keyword(std::move(while_keyword)),
      m_condition(std::move(condition)), m_body(std::move(body)) {}

syntax_kind while_statement_syntax::kind() const {
  return syntax_kind::while_statement;
}

std::vector<const syntax_node *> while_statement_syntax::children() const {
  return {
      &m_while_keyword,
      m_condition.get(),
      m_body.get(),
  };
}

const syntax_token &while_statement_syntax::while_keyword() const {
  return m_while_keyword;
}

const expression_syntax *while_statement_syntax::condition() const {
  return m_condition.get();
}

const statement_syntax *while_statement_syntax::body() const {
  return m_body.get();
}
