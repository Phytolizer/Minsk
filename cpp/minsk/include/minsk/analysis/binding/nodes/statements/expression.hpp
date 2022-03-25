#ifndef MINSK_ANALYSIS_BINDING_NODES_STATEMENTS_EXPRESSION_HPP
#define MINSK_ANALYSIS_BINDING_NODES_STATEMENTS_EXPRESSION_HPP

#include "minsk/analysis/binding/kind.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/binding/nodes/statement.hpp"
#include <memory>
namespace minsk::analysis::binding {

class bound_expression_statement final : public bound_statement {
  std::unique_ptr<bound_expression> m_expression;

public:
  explicit bound_expression_statement(
      std::unique_ptr<bound_expression> expression);

  bound_node_kind kind() const override;

  const bound_expression *expression() const;
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_NODES_STATEMENTS_EXPRESSION_HPP
