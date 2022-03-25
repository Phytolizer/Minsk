#ifndef MINSK_ANALYSIS_SYNTAX_EXPRESSIONS_NODES_ASSIGNMENT_HPP
#define MINSK_ANALYSIS_SYNTAX_EXPRESSIONS_NODES_ASSIGNMENT_HPP

#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/node.hpp"
#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include <memory>
#include <vector>
namespace minsk::analysis::syntax {

class assignment_expression_syntax final : public expression_syntax {
  syntax_token m_identifier_token;
  syntax_token m_equals_token;
  std::unique_ptr<expression_syntax> m_expression;

public:
  assignment_expression_syntax(syntax_token &&identifier_token,
                               syntax_token &&equals_token,
                               std::unique_ptr<expression_syntax> expression);

  syntax_kind kind() const override;
  std::vector<const node *> children() const override;
  const syntax_token &identifier_token() const;
  const syntax_token &equals_token() const;
  const expression_syntax *expression() const;
};

} // namespace minsk::analysis::syntax

#endif // MINSK_ANALYSIS_SYNTAX_EXPRESSIONS_NODES_ASSIGNMENT_HPP
