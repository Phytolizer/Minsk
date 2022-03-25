#ifndef MINSK_ANALYSIS_SYNTAX_NODES_UNIT_HPP
#define MINSK_ANALYSIS_SYNTAX_NODES_UNIT_HPP

#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/node.hpp"
#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include <memory>
#include <vector>

namespace minsk::analysis::syntax {

class compilation_unit_syntax final : public syntax_node {
  std::unique_ptr<expression_syntax> m_expression;
  syntax_token m_end_of_file_token;

public:
  compilation_unit_syntax(std::unique_ptr<expression_syntax> expression,
                          syntax_token &&end_of_file_token);

  syntax_kind kind() const override;
  std::vector<const syntax_node *> children() const override;

  const expression_syntax *expression() const;
  const syntax_token &end_of_file_token() const;
};

} // namespace minsk::analysis::syntax

#endif // MINSK_ANALYSIS_SYNTAX_NODES_UNIT_HPP
