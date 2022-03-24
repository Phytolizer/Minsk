#ifndef MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_BINARY_HPP
#define MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_BINARY_HPP

#include "minsk/analysis/binding/kind.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/binding/nodes/expressions/binary/operator.hpp"
#include "minsk/runtime/object.hpp"
#include <memory>

namespace minsk::analysis::binding {

class bound_binary_expression final : public bound_expression {
  std::unique_ptr<bound_expression> m_left;
  const bound_binary_operator *m_op;
  std::unique_ptr<bound_expression> m_right;

public:
  bound_binary_expression(std::unique_ptr<bound_expression> left,
                          const bound_binary_operator *op,
                          std::unique_ptr<bound_expression> right);
  bound_node_kind kind() const override;
  runtime::object_kind type() const override;
  const bound_expression *left() const;
  const bound_binary_operator *op() const;
  const bound_expression *right() const;
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_BINARY_HPP
