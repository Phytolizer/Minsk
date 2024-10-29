#include "minsk/analysis/syntax/nodes/statements/block.hpp"
#include "minsk/analysis/syntax/node.hpp"
#include "minsk/analysis/syntax/nodes/statement.hpp"
#include <algorithm>
#include <iterator>
#include <vector>

using minsk::analysis::syntax::block_statement_syntax;
using minsk::analysis::syntax::statement_syntax;
using minsk::analysis::syntax::syntax_kind;
using minsk::analysis::syntax::syntax_node;
using minsk::analysis::syntax::syntax_token;

block_statement_syntax::block_statement_syntax(
    syntax_token &&open_brace_token,
    std::vector<std::unique_ptr<statement_syntax>> &&statements,
    syntax_token &&close_brace_token)
    : m_open_brace_token(std::move(open_brace_token)),
      m_statements(std::move(statements)),
      m_close_brace_token(std::move(close_brace_token)) {}

syntax_kind block_statement_syntax::kind() const {
  return syntax_kind::block_statement;
}

std::vector<const syntax_node *> block_statement_syntax::children() const {
  std::vector<const syntax_node *> result;
  result.push_back(&m_open_brace_token);
  std::ranges::transform(
      m_statements, std::back_inserter(result),
      [](const std::unique_ptr<statement_syntax> &stmt) { return stmt.get(); });
  result.push_back(&m_close_brace_token);
  return result;
}

const syntax_token &block_statement_syntax::open_brace_token() const {
  return m_open_brace_token;
}

const std::vector<std::unique_ptr<statement_syntax>> &
block_statement_syntax::statements() const {
  return m_statements;
}

const syntax_token &block_statement_syntax::close_brace_token() const {
  return m_close_brace_token;
}
