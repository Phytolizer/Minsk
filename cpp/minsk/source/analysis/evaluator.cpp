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
#include "minsk/analysis/binding/nodes/statements/if.hpp"
#include "minsk/runtime/object.hpp"
#include <memory>
#include <stdexcept>

using minsk::analysis::evaluator;
using minsk::analysis::binding::bound_assignment_expression;
using minsk::analysis::binding::bound_binary_expression;
using minsk::analysis::binding::bound_binary_operator_kind;
using minsk::analysis::binding::bound_block_statement;
using minsk::analysis::binding::bound_expression;
using minsk::analysis::binding::bound_expression_statement;
using minsk::analysis::binding::bound_for_statement;
using minsk::analysis::binding::bound_if_statement;
using minsk::analysis::binding::bound_literal_expression;
using minsk::analysis::binding::bound_node_kind;
using minsk::analysis::binding::bound_statement;
using minsk::analysis::binding::bound_unary_expression;
using minsk::analysis::binding::bound_unary_operator_kind;
using minsk::analysis::binding::bound_variable_declaration;
using minsk::analysis::binding::bound_variable_expression;
using minsk::analysis::binding::bound_while_statement;
using minsk::runtime::object_ptr;

void evaluator::evaluate_statement(const bound_statement *root) {
  switch (root->kind()) {
  case bound_node_kind::block_statement:
    evaluate_block_statement(dynamic_cast<const bound_block_statement *>(root));
    break;
  case bound_node_kind::expression_statement:
    evaluate_expression_statement(
        dynamic_cast<const bound_expression_statement *>(root));
    break;
  case bound_node_kind::for_statement:
    evaluate_for_statement(dynamic_cast<const bound_for_statement *>(root));
    break;
  case bound_node_kind::if_statement:
    evaluate_if_statement(dynamic_cast<const bound_if_statement *>(root));
    break;
  case bound_node_kind::variable_declaration:
    evaluate_variable_declaration(
        dynamic_cast<const bound_variable_declaration *>(root));
    break;
  case bound_node_kind::while_statement:
    evaluate_while_statement(dynamic_cast<const bound_while_statement *>(root));
    break;
  default:
    throw std::runtime_error{
        fmt::format("unexpected node {}", magic_enum::enum_name(root->kind()))};
  }
}

void evaluator::evaluate_block_statement(const bound_block_statement *root) {
  for (const auto &stmt : root->statements()) {
    evaluate_statement(stmt.get());
  }
}

void evaluator::evaluate_expression_statement(
    const bound_expression_statement *root) {
  m_last_value = evaluate_expression(root->expression());
}

void evaluator::evaluate_if_statement(const bound_if_statement *root) {
  auto condition = evaluate_expression(root->condition());
  if (condition->as_boolean()->value()) {
    evaluate_statement(root->then_statement());
  } else if (root->else_statement() != nullptr) {
    evaluate_statement(root->else_statement());
  }
}

void evaluator::evaluate_for_statement(const bound_for_statement *root) {
  auto initial_value = evaluate_expression(root->initial_value());
  auto final_value = evaluate_expression(root->final_value());
  m_variables->emplace(root->variable(),
                       runtime::copy_object_ptr(initial_value.get()));

  while (true) {
    evaluate_statement(root->body());

    initial_value = std::make_unique<runtime::integer>(
        initial_value->as_integer()->value() + 1);
    auto it = m_variables->find(root->variable());
    if (it != m_variables->end()) {
      it->second = runtime::copy_object_ptr(initial_value.get());
    } else {
      m_variables->emplace(root->variable(),
                           runtime::copy_object_ptr(initial_value.get()));
    }
    if (initial_value->as_integer()->value() ==
        final_value->as_integer()->value()) {
      break;
    }
  }
}

void evaluator::evaluate_variable_declaration(
    const bound_variable_declaration *root) {
  auto initializer = evaluate_expression(root->initializer());
  m_variables->emplace(root->variable(),
                       runtime::copy_object_ptr(initializer.get()));
  m_last_value = std::move(initializer);
}

void evaluator::evaluate_while_statement(const bound_while_statement *root) {
  while (true) {
    auto condition = evaluate_expression(root->condition());
    if (!condition->as_boolean()->value()) {
      break;
    }
    evaluate_statement(root->body());
  }
}

object_ptr evaluator::evaluate_expression(const bound_expression *root) {
  switch (root->kind()) {
  case bound_node_kind::assignment_expression:
    return evaluate_assignment_expression(
        dynamic_cast<const bound_assignment_expression *>(root));
  case bound_node_kind::binary_expression:
    return evaluate_binary_expression(
        dynamic_cast<const bound_binary_expression *>(root));
  case bound_node_kind::literal_expression:
    return evaluate_literal_expression(
        dynamic_cast<const bound_literal_expression *>(root));
  case bound_node_kind::unary_expression:
    return evaluate_unary_expression(
        dynamic_cast<const bound_unary_expression *>(root));
  case bound_node_kind::variable_expression:
    return evaluate_variable_expression(
        dynamic_cast<const bound_variable_expression *>(root));
  default:
    throw std::runtime_error{
        fmt::format("unexpected node {}", magic_enum::enum_name(root->kind()))};
  }
}

object_ptr evaluator::evaluate_assignment_expression(
    const bound_assignment_expression *root) {
  auto value = evaluate_expression(root->expression());
  auto [_, inserted] = m_variables->emplace(
      root->variable(), runtime::copy_object_ptr(value.get()));
  if (!inserted) {
    (*m_variables)[root->variable()] = runtime::copy_object_ptr(value.get());
  }
  return std::move(value);
}

object_ptr evaluator::evaluate() {
  evaluate_statement(m_root);
  return std::move(m_last_value);
}

object_ptr
evaluator::evaluate_binary_expression(const bound_binary_expression *root) {
  runtime::object_ptr left = evaluate_expression(root->left());
  runtime::object_ptr right = evaluate_expression(root->right());
  switch (root->op()->kind()) {
  case bound_binary_operator_kind::addition:
    return std::make_unique<runtime::integer>(left->as_integer()->value() +
                                              right->as_integer()->value());
  case bound_binary_operator_kind::subtraction:
    return std::make_unique<runtime::integer>(left->as_integer()->value() -
                                              right->as_integer()->value());
  case bound_binary_operator_kind::multiplication:
    return std::make_unique<runtime::integer>(left->as_integer()->value() *
                                              right->as_integer()->value());
  case bound_binary_operator_kind::division:
    return std::make_unique<runtime::integer>(left->as_integer()->value() /
                                              right->as_integer()->value());
  case bound_binary_operator_kind::logical_and:
    return std::make_unique<runtime::boolean>(left->as_boolean()->value() &&
                                              right->as_boolean()->value());
  case bound_binary_operator_kind::logical_or:
    return std::make_unique<runtime::boolean>(left->as_boolean()->value() ||
                                              right->as_boolean()->value());
  case bound_binary_operator_kind::equality:
    return std::make_unique<runtime::boolean>(*left == *right);
  case bound_binary_operator_kind::inequality:
    return std::make_unique<runtime::boolean>(*left != *right);
  case bound_binary_operator_kind::less_than:
    return std::make_unique<runtime::boolean>(left->as_integer()->value() <
                                              right->as_integer()->value());
  case bound_binary_operator_kind::less_than_or_equal:
    return std::make_unique<runtime::boolean>(left->as_integer()->value() <=
                                              right->as_integer()->value());
  case bound_binary_operator_kind::greater_than:
    return std::make_unique<runtime::boolean>(left->as_integer()->value() >
                                              right->as_integer()->value());
  case bound_binary_operator_kind::greater_than_or_equal:
    return std::make_unique<runtime::boolean>(left->as_integer()->value() >=
                                              right->as_integer()->value());
  case bound_binary_operator_kind::bitwise_and:
    if (left->kind() == runtime::object_kind::integer) {
      return std::make_unique<runtime::integer>(left->as_integer()->value() &
                                                right->as_integer()->value());
    } else {
      return std::make_unique<runtime::boolean>(left->as_boolean()->value() &
                                                right->as_boolean()->value());
    }
  case bound_binary_operator_kind::bitwise_or:
    if (left->kind() == runtime::object_kind::integer) {
      return std::make_unique<runtime::integer>(left->as_integer()->value() |
                                                right->as_integer()->value());
    } else {
      return std::make_unique<runtime::boolean>(left->as_boolean()->value() |
                                                right->as_boolean()->value());
    }
  case bound_binary_operator_kind::bitwise_xor:
    if (left->kind() == runtime::object_kind::integer) {
      return std::make_unique<runtime::integer>(left->as_integer()->value() ^
                                                right->as_integer()->value());
    } else {
      return std::make_unique<runtime::boolean>(left->as_boolean()->value() ^
                                                right->as_boolean()->value());
    }
  }
  throw std::runtime_error{"corrupt operator kind"};
}

object_ptr
evaluator::evaluate_literal_expression(const bound_literal_expression *root) {
  return runtime::copy_object_ptr(root->value());
}

object_ptr
evaluator::evaluate_unary_expression(const bound_unary_expression *root) {
  runtime::object_ptr operand = evaluate_expression(root->operand());
  switch (root->op()->kind()) {
  case bound_unary_operator_kind::identity:
    return operand;
  case bound_unary_operator_kind::negation:
    return std::make_unique<runtime::integer>(-operand->as_integer()->value());
  case bound_unary_operator_kind::logical_negation:
    return std::make_unique<runtime::boolean>(!operand->as_boolean()->value());
  case bound_unary_operator_kind::bitwise_negation:
    return std::make_unique<runtime::integer>(~operand->as_integer()->value());
  }
  throw std::runtime_error{"corrupt operator kind"};
}

object_ptr
evaluator::evaluate_variable_expression(const bound_variable_expression *root) {
  return runtime::copy_object_ptr((*m_variables)[root->variable()].get());
}

evaluator::evaluator(const bound_statement *root, variable_map *variables)
    : m_root(root), m_variables(variables) {}
