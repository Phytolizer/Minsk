#ifndef MINSK_ANALYSIS_SYNTAX_NODES_STATEMENTS_VARIABLE_HPP
#define MINSK_ANALYSIS_SYNTAX_NODES_STATEMENTS_VARIABLE_HPP

#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/node.hpp"
#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/nodes/statement.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include <vector>
namespace minsk::analysis::syntax {

class variable_declaration_syntax final : public statement_syntax {
  syntax_token m_keyword_token;
  syntax_token m_identifier_token;
  syntax_token m_equals_token;
  std::unique_ptr<expression_syntax> m_initializer;

public:
  variable_declaration_syntax(syntax_token &&keyword_token,
                              syntax_token &&identifier_token,
                              syntax_token &&equals_token,
                              std::unique_ptr<expression_syntax> initializer);

  syntax_kind kind() const override;
  std::vector<const syntax_node *> children() const override;

  const syntax_token &keyword_token() const;
  const syntax_token &identifier_token() const;
  const syntax_token &equals_token() const;
  const expression_syntax *initializer() const;
};

} // namespace minsk::analysis::syntax

#endif // MINSK_ANALYSIS_SYNTAX_NODES_STATEMENTS_VARIABLE_HPP
