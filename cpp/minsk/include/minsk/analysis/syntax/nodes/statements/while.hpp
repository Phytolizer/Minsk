#ifndef MINSK_ANALYSIS_SYNTAX_NODES_STATEMENTS_WHILE_HPP
#define MINSK_ANALYSIS_SYNTAX_NODES_STATEMENTS_WHILE_HPP

#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/node.hpp"
#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/nodes/statement.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include <memory>
namespace minsk::analysis::syntax {

/// while <condition>
///   <body>
class while_statement_syntax final : public statement_syntax {
  syntax_token m_while_keyword;
  std::unique_ptr<expression_syntax> m_condition;
  std::unique_ptr<statement_syntax> m_body;

public:
  while_statement_syntax(syntax_token &&while_keyword,
                         std::unique_ptr<expression_syntax> condition,
                         std::unique_ptr<statement_syntax> body);

  syntax_kind kind() const override;
  std::vector<const syntax_node *> children() const override;

  const syntax_token &while_keyword() const;
  const expression_syntax *condition() const;
  const statement_syntax *body() const;
};

} // namespace minsk::analysis::syntax

#endif // MINSK_ANALYSIS_SYNTAX_NODES_STATEMENTS_WHILE_HPP
