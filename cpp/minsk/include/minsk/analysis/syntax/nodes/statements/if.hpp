#ifndef MINSK_ANALYSIS_SYNTAX_NODES_STATEMENTS_IF_HPP
#define MINSK_ANALYSIS_SYNTAX_NODES_STATEMENTS_IF_HPP

#include "minsk/analysis/syntax/nodes/else.hpp"
#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/nodes/statement.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include <memory>
#include <optional>

namespace minsk::analysis::syntax {

/// if x == y
///   z
/// else
///   w
class if_statement_syntax final : public statement_syntax {
  syntax_token m_keyword_token;
  std::unique_ptr<expression_syntax> m_condition;
  std::unique_ptr<statement_syntax> m_then_statement;
  std::unique_ptr<else_clause_syntax> m_else_clause;

public:
  if_statement_syntax(syntax_token &&keyword_token,
                      std::unique_ptr<expression_syntax> condition,
                      std::unique_ptr<statement_syntax> then_statement,
                      std::unique_ptr<else_clause_syntax> else_clause);

  syntax_kind kind() const override;
  std::vector<const syntax_node *> children() const override;

  const syntax_token &keyword_token() const;
  const expression_syntax *condition() const;
  const statement_syntax *then_statement() const;
  const minsk::analysis::syntax::else_clause_syntax *
else_clause() const;
};

} // namespace minsk::analysis::syntax

#endif // MINSK_ANALYSIS_SYNTAX_NODES_STATEMENTS_IF_HPP
