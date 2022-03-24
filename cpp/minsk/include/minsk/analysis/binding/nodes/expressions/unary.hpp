#ifndef MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_UNARY_HPP
#define MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_UNARY_HPP

#include "minsk/analysis/binding/kind.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/binding/nodes/expressions/unary/operator.hpp"
#include "minsk/runtime/object.hpp"
#include <memory>
namespace minsk::analysis::binding {

class bound_unary_expression final : public bound_expression {
  const bound_unary_operator *m_op;
  std::unique_ptr<bound_expression> m_operand;

public:
  bound_unary_expression(const bound_unary_operator *op,
                         std::unique_ptr<bound_expression> operand);

  bound_node_kind kind() const override;
  runtime::object_kind type() const override;
  const bound_unary_operator *op() const;
  const bound_expression *operand() const;
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_UNARY_HPP
