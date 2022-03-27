#ifndef MINSK_ANALYSIS_BINDING_NODES_STATEMENTS_WHILE_HPP
#define MINSK_ANALYSIS_BINDING_NODES_STATEMENTS_WHILE_HPP

#include "minsk/analysis/binding/kind.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/binding/nodes/statement.hpp"
#include <memory>

namespace minsk::analysis::binding {

class bound_while_statement final : public bound_statement {
  std::unique_ptr<bound_expression> m_condition;
  std::unique_ptr<bound_statement> m_body;

public:
  bound_while_statement(std::unique_ptr<bound_expression> condition,
                        std::unique_ptr<bound_statement> body);

  bound_node_kind kind() const override;

  const bound_expression *condition() const;
  const bound_statement *body() const;
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_NODES_STATEMENTS_WHILE_HPP
