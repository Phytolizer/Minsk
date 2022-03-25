#include "minsk/analysis/syntax/nodes/unit.hpp"
#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/nodes/statement.hpp"

minsk::analysis::syntax::compilation_unit_syntax::compilation_unit_syntax(
    std::unique_ptr<statement_syntax> statement,
    syntax_token &&end_of_file_token)
    : m_statement(std::move(statement)),
      m_end_of_file_token(std::move(end_of_file_token)) {}

minsk::analysis::syntax::syntax_kind
minsk::analysis::syntax::compilation_unit_syntax::kind() const {
  return syntax_kind::compilation_unit;
}

std::vector<const minsk::analysis::syntax::syntax_node *>
minsk::analysis::syntax::compilation_unit_syntax::children() const {
  return {
      m_statement.get(),
      &m_end_of_file_token,
  };
}

const minsk::analysis::syntax::statement_syntax *
minsk::analysis::syntax::compilation_unit_syntax::statement() const {
  return m_statement.get();
}

const minsk::analysis::syntax::syntax_token &
minsk::analysis::syntax::compilation_unit_syntax::end_of_file_token() const {
  return m_end_of_file_token;
}
