#include "minsk/analysis/syntax/nodes/statements/for.hpp"

minsk::analysis::syntax::for_statement_syntax::for_statement_syntax(
    syntax_token &&for_keyword, syntax_token &&identifier_token,
    syntax_token &&equals_token,
    std::unique_ptr<expression_syntax> initial_value, syntax_token &&to_keyword,
    std::unique_ptr<expression_syntax> final_value,
    std::unique_ptr<statement_syntax> body)
    : m_for_keyword(std::move(for_keyword)),
      m_identifier_token(std::move(identifier_token)),
      m_equals_token(std::move(equals_token)),
      m_initial_value(std::move(initial_value)),
      m_to_keyword(std::move(to_keyword)),
      m_final_value(std::move(final_value)), m_body(std::move(body)) {}

minsk::analysis::syntax::syntax_kind
minsk::analysis::syntax::for_statement_syntax::kind() const {
  return syntax_kind::for_statement;
}

std::vector<const minsk::analysis::syntax::syntax_node *>
minsk::analysis::syntax::for_statement_syntax::children() const {
  return {
      &m_for_keyword,        &m_identifier_token, &m_equals_token,
      m_initial_value.get(), &m_to_keyword,       m_final_value.get(),
      m_body.get(),
  };
}

const minsk::analysis::syntax::syntax_token &
minsk::analysis::syntax::for_statement_syntax::for_keyword() const {
  return m_for_keyword;
}

const minsk::analysis::syntax::syntax_token &
minsk::analysis::syntax::for_statement_syntax::identifier_token() const {
  return m_identifier_token;
}

const minsk::analysis::syntax::syntax_token &
minsk::analysis::syntax::for_statement_syntax::equals_token() const {
  return m_equals_token;
}

const minsk::analysis::syntax::expression_syntax *
minsk::analysis::syntax::for_statement_syntax::initial_value() const {
  return m_initial_value.get();
}

const minsk::analysis::syntax::syntax_token &
minsk::analysis::syntax::for_statement_syntax::to_keyword() const {
  return m_to_keyword;
}

const minsk::analysis::syntax::expression_syntax *
minsk::analysis::syntax::for_statement_syntax::final_value() const {
  return m_final_value.get();
}

const minsk::analysis::syntax::statement_syntax *
minsk::analysis::syntax::for_statement_syntax::body() const {
  return m_body.get();
}
