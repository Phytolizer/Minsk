#ifndef MINSK_ANALYSIS_SYNTAX_NODES_ELSE_HPP
#define MINSK_ANALYSIS_SYNTAX_NODES_ELSE_HPP

#include "minsk/analysis/syntax/node.hpp"
#include "minsk/analysis/syntax/nodes/statement.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include <memory>
namespace minsk::analysis::syntax {

class else_clause_syntax final : public syntax_node {
  syntax_token m_keyword_token;
  std::unique_ptr<statement_syntax> m_else_statement;

public:
  else_clause_syntax(syntax_token &&keyword_token,
                     std::unique_ptr<statement_syntax> else_statement);

  syntax_kind kind() const override;
  std::vector<const syntax_node *> children() const override;

  const syntax_token &keyword_token() const;
  const statement_syntax *else_statement() const;
};

} // namespace minsk::analysis::syntax

#endif // MINSK_ANALYSIS_SYNTAX_NODES_ELSE_HPP
