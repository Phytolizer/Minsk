#include "minsk/analysis/binding/nodes/expressions/unary.hpp"
#include "minsk/analysis/binding/kind.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/binding/nodes/expressions/unary/operator.hpp"

minsk::analysis::binding::bound_unary_expression::bound_unary_expression(
    const bound_unary_operator *op, std::unique_ptr<bound_expression> operand)
    : m_op(op), m_operand(std::move(operand)) {}

minsk::analysis::binding::bound_node_kind
minsk::analysis::binding::bound_unary_expression::kind() const {
  return bound_node_kind::unary_expression;
}
minsk::runtime::object_kind
minsk::analysis::binding::bound_unary_expression::type() const {
  return m_op->result_type();
}
const minsk::analysis::binding::bound_unary_operator *
minsk::analysis::binding::bound_unary_expression::op() const {
  return m_op;
}
const minsk::analysis::binding::bound_expression *
minsk::analysis::binding::bound_unary_expression::operand() const {
  return m_operand.get();
}
