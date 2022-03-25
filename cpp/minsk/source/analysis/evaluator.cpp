#include "minsk/analysis/evaluator.hpp"
#include "fmt/format.h"
#include "minsk/analysis/binding/kind.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/binding/nodes/expressions/assignment.hpp"
#include "minsk/analysis/binding/nodes/expressions/binary.hpp"
#include "minsk/analysis/binding/nodes/expressions/binary/kind.hpp"
#include "minsk/analysis/binding/nodes/expressions/literal.hpp"
#include "minsk/analysis/binding/nodes/expressions/unary.hpp"
#include "minsk/analysis/binding/nodes/expressions/unary/kind.hpp"
#include "minsk/analysis/binding/nodes/expressions/variable.hpp"
#include "minsk/analysis/binding/nodes/statements/block.hpp"
#include "minsk/analysis/binding/nodes/statements/expression.hpp"
#include "minsk/runtime/object.hpp"
#include <memory>
#include <stdexcept>

void minsk::analysis::evaluator::evaluate_statement(
    const binding::bound_statement *root) {
  switch (root->kind()) {
  case binding::bound_node_kind::block_statement:
    evaluate_block_statement(
        dynamic_cast<const binding::bound_block_statement *>(root));
    break;
  case binding::bound_node_kind::expression_statement:
    evaluate_expression_statement(
        dynamic_cast<const binding::bound_expression_statement *>(root));
    break;
  case binding::bound_node_kind::variable_declaration:
    evaluate_variable_declaration(
        dynamic_cast<const binding::bound_variable_declaration *>(root));
    break;
  default:
    throw std::runtime_error{
        fmt::format("unexpected node {}", magic_enum::enum_name(root->kind()))};
  }
}

void minsk::analysis::evaluator::evaluate_block_statement(
    const binding::bound_block_statement *root) {
  for (const auto &stmt : root->statements()) {
    evaluate_statement(stmt.get());
  }
}

void minsk::analysis::evaluator::evaluate_expression_statement(
    const binding::bound_expression_statement *root) {
  m_last_value = evaluate_expression(root->expression());
}

void minsk::analysis::evaluator::evaluate_variable_declaration(
    const binding::bound_variable_declaration *root) {
  auto initializer = evaluate_expression(root->initializer());
  m_variables->emplace(root->variable(),
                       runtime::copy_object_ptr(initializer.get()));
  m_last_value = std::move(initializer);
}

minsk::runtime::object_ptr minsk::analysis::evaluator::evaluate_expression(
    const minsk::analysis::binding::bound_expression *root) {
  switch (root->kind()) {
  case binding::bound_node_kind::assignment_expression:
    return evaluate_assignment_expression(
        dynamic_cast<const binding::bound_assignment_expression *>(root));
  case binding::bound_node_kind::binary_expression:
    return evaluate_binary_expression(
        dynamic_cast<const binding::bound_binary_expression *>(root));
  case binding::bound_node_kind::literal_expression:
    return evaluate_literal_expression(
        dynamic_cast<const binding::bound_literal_expression *>(root));
  case binding::bound_node_kind::unary_expression:
    return evaluate_unary_expression(
        dynamic_cast<const binding::bound_unary_expression *>(root));
  case binding::bound_node_kind::variable_expression:
    return evaluate_variable_expression(
        dynamic_cast<const binding::bound_variable_expression *>(root));
  default:
    throw std::runtime_error{
        fmt::format("unexpected node {}", magic_enum::enum_name(root->kind()))};
  }
}
minsk::runtime::object_ptr
minsk::analysis::evaluator::evaluate_assignment_expression(
    const binding::bound_assignment_expression *root) {
  auto value = evaluate_expression(root->expression());
  auto [_, inserted] = m_variables->emplace(
      root->variable(), runtime::copy_object_ptr(value.get()));
  if (!inserted) {
    (*m_variables)[root->variable()] = runtime::copy_object_ptr(value.get());
  }
  return std::move(value);
}
minsk::runtime::object_ptr minsk::analysis::evaluator::evaluate() {
  evaluate_statement(m_root);
  return std::move(m_last_value);
}
minsk::runtime::object_ptr
minsk::analysis::evaluator::evaluate_binary_expression(
    const binding::bound_binary_expression *root) {
  runtime::object_ptr left = evaluate_expression(root->left());
  runtime::object_ptr right = evaluate_expression(root->right());
  switch (root->op()->kind()) {
  case binding::bound_binary_operator_kind::addition:
    return std::make_unique<runtime::integer>(left->as_integer()->value() +
                                              right->as_integer()->value());
  case binding::bound_binary_operator_kind::subtraction:
    return std::make_unique<runtime::integer>(left->as_integer()->value() -
                                              right->as_integer()->value());
  case binding::bound_binary_operator_kind::multiplication:
    return std::make_unique<runtime::integer>(left->as_integer()->value() *
                                              right->as_integer()->value());
  case binding::bound_binary_operator_kind::division:
    return std::make_unique<runtime::integer>(left->as_integer()->value() /
                                              right->as_integer()->value());
  case binding::bound_binary_operator_kind::logical_and:
    return std::make_unique<runtime::boolean>(left->as_boolean()->value() &&
                                              right->as_boolean()->value());
  case binding::bound_binary_operator_kind::logical_or:
    return std::make_unique<runtime::boolean>(left->as_boolean()->value() ||
                                              right->as_boolean()->value());
  case binding::bound_binary_operator_kind::equality:
    return std::make_unique<runtime::boolean>(*left == *right);
  case binding::bound_binary_operator_kind::inequality:
    return std::make_unique<runtime::boolean>(*left != *right);
  }
  throw std::runtime_error{"corrupt operator kind"};
}
minsk::runtime::object_ptr
minsk::analysis::evaluator::evaluate_literal_expression(
    const binding::bound_literal_expression *root) {
  return runtime::copy_object_ptr(root->value());
}
minsk::runtime::object_ptr
minsk::analysis::evaluator::evaluate_unary_expression(
    const binding::bound_unary_expression *root) {
  runtime::object_ptr operand = evaluate_expression(root->operand());
  switch (root->op()->kind()) {
  case binding::bound_unary_operator_kind::identity:
    return operand;
  case binding::bound_unary_operator_kind::negation:
    return std::make_unique<runtime::integer>(-operand->as_integer()->value());
  case binding::bound_unary_operator_kind::logical_negation:
    return std::make_unique<runtime::boolean>(!operand->as_boolean()->value());
  }
  throw std::runtime_error{"corrupt operator kind"};
}

minsk::runtime::object_ptr
minsk::analysis::evaluator::evaluate_variable_expression(
    const binding::bound_variable_expression *root) {
  return runtime::copy_object_ptr((*m_variables)[root->variable()].get());
}

minsk::analysis::evaluator::evaluator(const binding::bound_statement *root,
                                      variable_map *variables)
    : m_root(root), m_variables(variables) {}
