#ifndef MINSK_ANALYSIS_SYNTAX_NODES_EXPRESSIONS_NAME_HPP
#define MINSK_ANALYSIS_SYNTAX_NODES_EXPRESSIONS_NAME_HPP

#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/node.hpp"
#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include <vector>

namespace minsk::analysis::syntax {

class name_expression_syntax final : public expression_syntax {
  syntax_token m_identifier_token;

public:
  explicit name_expression_syntax(syntax_token &&identifier_token);

  syntax_kind kind() const override;
  std::vector<const syntax_node *> children() const override;
  const syntax_token &identifier_token() const;
};

} // namespace minsk::analysis::syntax

#endif // MINSK_ANALYSIS_SYNTAX_NODES_EXPRESSIONS_NAME_HPP
