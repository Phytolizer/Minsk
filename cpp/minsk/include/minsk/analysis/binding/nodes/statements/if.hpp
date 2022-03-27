#ifndef MINSK_ANALYSIS_BINDING_NODES_STATEMENTS_IF_HPP
#define MINSK_ANALYSIS_BINDING_NODES_STATEMENTS_IF_HPP

#include "minsk/analysis/binding/kind.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/binding/nodes/statement.hpp"
#include <memory>
namespace minsk::analysis::binding {

class bound_if_statement final : public bound_statement {
  std::unique_ptr<bound_expression> m_condition;
  std::unique_ptr<bound_statement> m_then_statement;
  std::unique_ptr<bound_statement> m_else_statement;

public:
  bound_if_statement(std::unique_ptr<bound_expression> condition,
                     std::unique_ptr<bound_statement> then_statement,
                     std::unique_ptr<bound_statement> else_statement);

  bound_node_kind kind() const override;

  const bound_expression *condition() const;
  const bound_statement *then_statement() const;
  const bound_statement *else_statement() const;
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_NODES_STATEMENTS_IF_HPP
