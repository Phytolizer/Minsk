#include "minsk/analysis/evaluator.hpp"
#include "fmt/format.h"
#include "minsk/analysis/binding/kind.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/binding/nodes/expressions/binary.hpp"
#include "minsk/analysis/binding/nodes/expressions/binary/kind.hpp"
#include "minsk/analysis/binding/nodes/expressions/literal.hpp"
#include "minsk/analysis/binding/nodes/expressions/unary.hpp"
#include "minsk/analysis/binding/nodes/expressions/unary/kind.hpp"
#include <stdexcept>
minsk::analysis::evaluator::evaluator(
    const minsk::analysis::binding::bound_expression *root)
    : m_root(root) {}
int minsk::analysis::evaluator::evaluate_expression(
    const minsk::analysis::binding::bound_expression *root) const {
  switch (root->kind()) {
  case binding::bound_node_kind::binary_expression:
    return evaluate_binary_expression(
        dynamic_cast<const binding::bound_binary_expression *>(root));
  case binding::bound_node_kind::literal_expression:
    return evaluate_literal_expression(
        dynamic_cast<const binding::bound_literal_expression *>(root));
  case binding::bound_node_kind::unary_expression:
    return evaluate_unary_expression(
        dynamic_cast<const binding::bound_unary_expression *>(root));
  default:
    throw std::runtime_error{fmt::format("unexpected syntax {}",
                                         magic_enum::enum_name(root->kind()))};
  }
}
int minsk::analysis::evaluator::evaluate() const {
  return evaluate_expression(m_root);
}
int minsk::analysis::evaluator::evaluate_binary_expression(
    const binding::bound_binary_expression *root) const {
  int left = evaluate_expression(root->left());
  int right = evaluate_expression(root->right());
  switch (root->op()->kind()) {
  case binding::bound_binary_operator_kind::addition:
    return left + right;
  case binding::bound_binary_operator_kind::subtraction:
    return left - right;
  case binding::bound_binary_operator_kind::multiplication:
    return left * right;
  case binding::bound_binary_operator_kind::division:
    return left / right;
  }
  throw std::runtime_error{"corrupt operator kind"};
}
int minsk::analysis::evaluator::evaluate_literal_expression(
    const binding::bound_literal_expression *root) const {
  return dynamic_cast<const runtime::integer *>(root->value())->value();
}
int minsk::analysis::evaluator::evaluate_unary_expression(
    const binding::bound_unary_expression *root) const {
  int operand = evaluate_expression(root->operand());
  switch (root->op()->kind()) {
  case binding::bound_unary_operator_kind::identity:
    return operand;
  case binding::bound_unary_operator_kind::negation:
    return -operand;
  }
  throw std::runtime_error{"corrupt operator kind"};
}
