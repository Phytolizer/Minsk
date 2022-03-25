#ifndef MINSK_ANALYSIS_SYNTAX_NODES_STATEMENTS_BLOCK_HPP
#define MINSK_ANALYSIS_SYNTAX_NODES_STATEMENTS_BLOCK_HPP

#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/node.hpp"
#include "minsk/analysis/syntax/nodes/statement.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include <memory>
#include <vector>
namespace minsk::analysis::syntax {

class block_statement_syntax final : public statement_syntax {
  syntax_token m_open_brace_token;
  std::vector<std::unique_ptr<statement_syntax>> m_statements;
  syntax_token m_close_brace_token;

public:
  block_statement_syntax(
      syntax_token &&open_brace_token,
      std::vector<std::unique_ptr<statement_syntax>> &&statements,
      syntax_token &&close_brace_token);

  syntax_kind kind() const override;
  std::vector<const syntax_node *> children() const override;

  const syntax_token &open_brace_token() const;
  const std::vector<std::unique_ptr<statement_syntax>> &statements() const;
  const syntax_token &close_brace_token() const;
};

} // namespace minsk::analysis::syntax

#endif // MINSK_ANALYSIS_SYNTAX_NODES_STATEMENTS_BLOCK_HPP
