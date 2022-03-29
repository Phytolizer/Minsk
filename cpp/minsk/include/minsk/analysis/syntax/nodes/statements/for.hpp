#ifndef MINSK_ANALYSIS_SYNTAX_NODES_STATEMENTS_FOR_HPP
#define MINSK_ANALYSIS_SYNTAX_NODES_STATEMENTS_FOR_HPP

#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/node.hpp"
#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/nodes/statement.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include <memory>
#include <vector>
namespace minsk::analysis::syntax {

class for_statement_syntax final : public statement_syntax {
  syntax_token m_for_keyword;
  syntax_token m_identifier_token;
  syntax_token m_equals_token;
  std::unique_ptr<expression_syntax> m_initial_value;
  syntax_token m_to_keyword;
  std::unique_ptr<expression_syntax> m_final_value;
  std::unique_ptr<statement_syntax> m_body;

public:
  for_statement_syntax(syntax_token &&for_keyword,
                       syntax_token &&identifier_token,
                       syntax_token &&equals_token,
                       std::unique_ptr<expression_syntax> initial_value,
                       syntax_token &&to_keyword,
                       std::unique_ptr<expression_syntax> final_value,
                       std::unique_ptr<statement_syntax> body);

  syntax_kind kind() const override;
  std::vector<const syntax_node *> children() const override;

  const syntax_token &for_keyword() const;
  const syntax_token &identifier_token() const;
  const syntax_token &equals_token() const;
  const expression_syntax *initial_value() const;
  const syntax_token &to_keyword() const;
  const expression_syntax *final_value() const;
  const statement_syntax *body() const;
};

} // namespace minsk::analysis::syntax

#endif // MINSK_ANALYSIS_SYNTAX_NODES_STATEMENTS_FOR_HPP
