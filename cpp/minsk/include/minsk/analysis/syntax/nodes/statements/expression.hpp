#ifndef MINSK_ANALYSIS_SYNTAX_NODES_STATEMENTS_EXPRESSION_HPP
#define MINSK_ANALYSIS_SYNTAX_NODES_STATEMENTS_EXPRESSION_HPP

#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/node.hpp"
#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/nodes/statement.hpp"
#include <memory>
#include <vector>
namespace minsk::analysis::syntax {

class expression_statement_syntax final : public statement_syntax {
  std::unique_ptr<expression_syntax> m_expression;

public:
  explicit expression_statement_syntax(
      std::unique_ptr<expression_syntax> expression);

  syntax_kind kind() const override;
  std::vector<const syntax_node *> children() const override;

  const expression_syntax *expression() const;
};

} // namespace minsk::analysis::syntax

#endif // MINSK_ANALYSIS_SYNTAX_NODES_STATEMENTS_EXPRESSION_HPP
