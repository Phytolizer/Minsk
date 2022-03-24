#include "minsk/analysis/binding/nodes/expressions/binary.hpp"
#include "minsk/analysis/binding/kind.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/binding/nodes/expressions/binary/operator.hpp"
#include "minsk/runtime/object.hpp"

minsk::analysis::binding::bound_binary_expression::bound_binary_expression(
    std::unique_ptr<bound_expression> left, const bound_binary_operator *op,
    std::unique_ptr<bound_expression> right)
    : m_left(std::move(left)), m_op(op), m_right(std::move(right)) {}

minsk::analysis::binding::bound_node_kind
minsk::analysis::binding::bound_binary_expression::kind() const {
  return bound_node_kind::binary_expression;
}
minsk::runtime::object_kind
minsk::analysis::binding::bound_binary_expression::type() const {
  return m_op->result_type();
}
const minsk::analysis::binding::bound_expression *
minsk::analysis::binding::bound_binary_expression::left() const {
  return m_left.get();
}
const minsk::analysis::binding::bound_binary_operator *
minsk::analysis::binding::bound_binary_expression::op() const {
  return m_op;
}
const minsk::analysis::binding::bound_expression *
minsk::analysis::binding::bound_binary_expression::right() const {
  return m_right.get();
}
