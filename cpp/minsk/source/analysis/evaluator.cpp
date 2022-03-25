#include "minsk/analysis/evaluator.hpp"
#include "fmt/format.h"
#include "minsk/analysis/binding/kind.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/binding/nodes/expressions/binary.hpp"
#include "minsk/analysis/binding/nodes/expressions/binary/kind.hpp"
#include "minsk/analysis/binding/nodes/expressions/literal.hpp"
#include "minsk/analysis/binding/nodes/expressions/unary.hpp"
#include "minsk/analysis/binding/nodes/expressions/unary/kind.hpp"
#include "minsk/runtime/object.hpp"
#include <memory>
#include <stdexcept>
minsk::analysis::evaluator::evaluator(
    const minsk::analysis::binding::bound_expression *root)
    : m_root(root) {}
minsk::runtime::object_ptr minsk::analysis::evaluator::evaluate_expression(
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
minsk::runtime::object_ptr minsk::analysis::evaluator::evaluate() const {
  return evaluate_expression(m_root);
}
minsk::runtime::object_ptr
minsk::analysis::evaluator::evaluate_binary_expression(
    const binding::bound_binary_expression *root) const {
  runtime::object_ptr left = evaluate_expression(root->left());
  runtime::object_ptr right = evaluate_expression(root->right());
  switch (root->op()->kind()) {
  case binding::bound_binary_operator_kind::addition:
    return std::make_unique<runtime::integer>(
        dynamic_cast<runtime::integer *>(left.get())->value() +
        dynamic_cast<runtime::integer *>(right.get())->value());
  case binding::bound_binary_operator_kind::subtraction:
    return std::make_unique<runtime::integer>(
        dynamic_cast<runtime::integer *>(left.get())->value() -
        dynamic_cast<runtime::integer *>(right.get())->value());
  case binding::bound_binary_operator_kind::multiplication:
    return std::make_unique<runtime::integer>(
        dynamic_cast<runtime::integer *>(left.get())->value() *
        dynamic_cast<runtime::integer *>(right.get())->value());
  case binding::bound_binary_operator_kind::division:
    return std::make_unique<runtime::integer>(
        dynamic_cast<runtime::integer *>(left.get())->value() /
        dynamic_cast<runtime::integer *>(right.get())->value());
  case binding::bound_binary_operator_kind::logical_and:
    return std::make_unique<runtime::boolean>(
        dynamic_cast<runtime::boolean *>(left.get())->value() &&
        dynamic_cast<runtime::boolean *>(right.get())->value());
  case binding::bound_binary_operator_kind::logical_or:
    return std::make_unique<runtime::boolean>(
        dynamic_cast<runtime::boolean *>(left.get())->value() ||
        dynamic_cast<runtime::boolean *>(right.get())->value());
  }
  throw std::runtime_error{"corrupt operator kind"};
}
minsk::runtime::object_ptr
minsk::analysis::evaluator::evaluate_literal_expression(
    const binding::bound_literal_expression *root) const {
  return runtime::copy_object_ptr(root->value());
}
minsk::runtime::object_ptr
minsk::analysis::evaluator::evaluate_unary_expression(
    const binding::bound_unary_expression *root) const {
  runtime::object_ptr operand = evaluate_expression(root->operand());
  switch (root->op()->kind()) {
  case binding::bound_unary_operator_kind::identity:
    return operand;
  case binding::bound_unary_operator_kind::negation:
    return std::make_unique<runtime::integer>(
        -dynamic_cast<runtime::integer *>(operand.get())->value());
  case binding::bound_unary_operator_kind::logical_negation:
    return std::make_unique<runtime::boolean>(
        !dynamic_cast<runtime::boolean *>(operand.get())->value());
  }
  throw std::runtime_error{"corrupt operator kind"};
}
