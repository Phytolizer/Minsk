#include "minsk/analysis/binding/binder.hpp"
#include "fmt/core.h"
#include "magic_enum.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/binding/nodes/expressions/assignment.hpp"
#include "minsk/analysis/binding/nodes/expressions/binary.hpp"
#include "minsk/analysis/binding/nodes/expressions/binary/operator.hpp"
#include "minsk/analysis/binding/nodes/expressions/literal.hpp"
#include "minsk/analysis/binding/nodes/expressions/unary.hpp"
#include "minsk/analysis/binding/nodes/expressions/unary/operator.hpp"
#include "minsk/analysis/binding/nodes/expressions/variable.hpp"
#include "minsk/analysis/binding/nodes/statement.hpp"
#include "minsk/analysis/binding/nodes/statements/block.hpp"
#include "minsk/analysis/binding/nodes/statements/expression.hpp"
#include "minsk/analysis/binding/nodes/statements/variable.hpp"
#include "minsk/analysis/binding/scope.hpp"
#include "minsk/analysis/binding/scope/global.hpp"
#include "minsk/analysis/diagnostic.hpp"
#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/nodes/expressions/assignment.hpp"
#include "minsk/analysis/syntax/nodes/expressions/binary.hpp"
#include "minsk/analysis/syntax/nodes/expressions/literal.hpp"
#include "minsk/analysis/syntax/nodes/expressions/name.hpp"
#include "minsk/analysis/syntax/nodes/expressions/parenthesized.hpp"
#include "minsk/analysis/syntax/nodes/expressions/unary.hpp"
#include "minsk/analysis/syntax/nodes/statements/block.hpp"
#include "minsk/analysis/syntax/nodes/statements/variable.hpp"
#include "minsk/analysis/variable_symbol.hpp"
#include "minsk/runtime/object.hpp"
#include <algorithm>
#include <bits/ranges_algobase.h>
#include <iterator>
#include <memory>
#include <ranges>
#include <stack>
#include <stdexcept>

std::unique_ptr<minsk::analysis::binding::bound_expression>
minsk::analysis::binding::binder::bind_assignment_expression(
    const syntax::assignment_expression_syntax *syntax) {
  auto expression = bind_expression(syntax->expression());
  auto name = syntax->identifier_token().text();
  auto variable = m_scope->try_lookup(name);
  if (!variable) {
    m_diagnostics.report_undefined_name(syntax->identifier_token().span(),
                                        name);
    return expression;
  }

  if (variable->is_read_only()) {
    m_diagnostics.report_cannot_assign(syntax->equals_token().span(), name);
  }

  if (expression->type() != variable->type()) {
    m_diagnostics.report_cannot_convert(syntax->expression()->span(),
                                        expression->type(), variable->type());
    return expression;
  }

  return std::make_unique<bound_assignment_expression>(std::move(*variable),
                                                       std::move(expression));
}

std::unique_ptr<minsk::analysis::binding::bound_expression>
minsk::analysis::binding::binder::bind_binary_expression(
    const syntax::binary_expression_syntax *syntax) {
  std::unique_ptr<bound_expression> left = bind_expression(syntax->left());
  std::unique_ptr<bound_expression> right = bind_expression(syntax->right());

  const bound_binary_operator *op = bound_binary_operator::bind(
      syntax->operator_token().kind(), left->type(), right->type());
  if (op == nullptr) {
    m_diagnostics.report_undefined_binary_operator(
        syntax->operator_token().span(), syntax->operator_token().text(),
        left->type(), right->type());
    return left;
  }

  return std::make_unique<bound_binary_expression>(std::move(left), op,
                                                   std::move(right));
}

std::unique_ptr<minsk::analysis::binding::bound_expression>
minsk::analysis::binding::binder::bind_literal_expression(
    const syntax::literal_expression_syntax *syntax) {
  return std::make_unique<bound_literal_expression>(
      runtime::copy_object_ptr(syntax->value()));
}

std::unique_ptr<minsk::analysis::binding::bound_expression>
minsk::analysis::binding::binder::bind_name_expression(
    const syntax::name_expression_syntax *syntax) {
  auto name = syntax->identifier_token().text();
  auto variable = m_scope->try_lookup(name);
  if (!variable) {
    m_diagnostics.report_undefined_name(syntax->identifier_token().span(),
                                        name);
    return std::make_unique<bound_literal_expression>(
        std::make_unique<runtime::integer>(0));
  }

  auto type = variable->type();
  return std::make_unique<bound_variable_expression>(std::move(*variable));
}

std::unique_ptr<minsk::analysis::binding::bound_expression>
minsk::analysis::binding::binder::bind_parenthesized_expression(
    const syntax::parenthesized_expression_syntax *syntax) {
  return bind_expression(syntax->expression());
}

std::unique_ptr<minsk::analysis::binding::bound_expression>
minsk::analysis::binding::binder::bind_unary_expression(
    const syntax::unary_expression_syntax *syntax) {
  std::unique_ptr<bound_expression> operand =
      bind_expression(syntax->operand());
  const bound_unary_operator *op = bound_unary_operator::bind(
      syntax->operator_token().kind(), operand->type());
  if (op == nullptr) {
    m_diagnostics.report_undefined_unary_operator(
        syntax->operator_token().span(), syntax->operator_token().text(),
        operand->type());
    return operand;
  }

  return std::make_unique<bound_unary_expression>(op, std::move(operand));
}

std::unique_ptr<minsk::analysis::binding::bound_scope>
minsk::analysis::binding::binder::create_parent_scope(
    bound_global_scope *previous) {
  auto stack = std::stack<bound_global_scope *>{};
  while (previous != nullptr) {
    stack.push(previous);
    previous = previous->previous();
  }

  std::unique_ptr<bound_scope> current;

  while (!stack.empty()) {
    auto global_scope = stack.top();
    stack.pop();
    auto scope = std::make_unique<bound_scope>(std::move(current));
    for (const auto &v : global_scope->variables()) {
      scope->try_declare(variable_symbol{v});
    }
    current = std::move(scope);
  }

  return current;
}

minsk::analysis::binding::binder::binder(std::unique_ptr<bound_scope> parent)
    : m_scope(std::make_unique<bound_scope>(std::move(parent))) {}

minsk::analysis::binding::bound_global_scope
minsk::analysis::binding::binder::bind_global_scope(
    minsk::analysis::binding::bound_global_scope *previous,
    const syntax::compilation_unit_syntax *syntax) {
  auto parent_scope = create_parent_scope(previous);
  auto binder = binding::binder{std::move(parent_scope)};
  auto expression = binder.bind_statement(syntax->statement());
  auto variables = binder.m_scope->get_declared_variables();
  auto diagnostics = std::vector<diagnostic>{};

  if (previous != nullptr) {
    std::ranges::copy(previous->diagnostics(), std::back_inserter(diagnostics));
  }
  std::ranges::copy(binder.diagnostics(), std::back_inserter(diagnostics));

  return bound_global_scope{
      nullptr,
      std::move(diagnostics),
      std::move(variables),
      std::move(expression),
  };
}

std::unique_ptr<minsk::analysis::binding::bound_statement>
minsk::analysis::binding::binder::bind_statement(
    const syntax::statement_syntax *syntax) {
  switch (syntax->kind()) {
  case syntax::syntax_kind::block_statement:
    return bind_block_statement(
        dynamic_cast<const syntax::block_statement_syntax *>(syntax));
  case syntax::syntax_kind::expression_statement:
    return bind_expression_statement(
        dynamic_cast<const syntax::expression_statement_syntax *>(syntax));
  case syntax::syntax_kind::variable_declaration:
    return bind_variable_declaration(
        dynamic_cast<const syntax::variable_declaration_syntax *>(syntax));
  default:
    throw std::runtime_error{fmt::format(
        "Unexpected syntax {}", magic_enum::enum_name(syntax->kind()))};
  }
}

std::unique_ptr<minsk::analysis::binding::bound_statement>
minsk::analysis::binding::binder::bind_block_statement(
    const syntax::block_statement_syntax *syntax) {
  auto statements = std::vector<std::unique_ptr<bound_statement>>{};
  m_scope = std::make_unique<bound_scope>(std::move(m_scope));
  for (const auto &statement : syntax->statements()) {
    statements.emplace_back(bind_statement(statement.get()));
  }
  m_scope = m_scope->take_parent();
  return std::make_unique<bound_block_statement>(std::move(statements));
}

std::unique_ptr<minsk::analysis::binding::bound_statement>
minsk::analysis::binding::binder::bind_expression_statement(
    const syntax::expression_statement_syntax *syntax) {
  auto expression = bind_expression(syntax->expression());
  return std::make_unique<bound_expression_statement>(std::move(expression));
}

std::unique_ptr<minsk::analysis::binding::bound_statement>
minsk::analysis::binding::binder::bind_variable_declaration(
    const syntax::variable_declaration_syntax *syntax) {
  auto name = syntax->identifier_token().text();
  auto is_read_only =
      syntax->keyword_token().kind() == syntax::syntax_kind::let_keyword;
  auto initializer = bind_expression(syntax->initializer());
  auto variable =
      variable_symbol{std::string{name}, is_read_only, initializer->type()};
  if (!m_scope->try_declare(variable_symbol{variable})) {
    m_diagnostics.report_variable_already_declared(
        syntax->identifier_token().span(), name);
  }
  return std::make_unique<bound_variable_declaration>(std::move(variable),
                                                      std::move(initializer));
}

std::unique_ptr<minsk::analysis::binding::bound_expression>
minsk::analysis::binding::binder::bind_expression(
    const syntax::expression_syntax *syntax) {
  switch (syntax->kind()) {
  case syntax::syntax_kind::assignment_expression:
    return bind_assignment_expression(
        dynamic_cast<const syntax::assignment_expression_syntax *>(syntax));
  case syntax::syntax_kind::binary_expression:
    return bind_binary_expression(
        dynamic_cast<const syntax::binary_expression_syntax *>(syntax));
  case syntax::syntax_kind::literal_expression:
    return bind_literal_expression(
        dynamic_cast<const syntax::literal_expression_syntax *>(syntax));
  case syntax::syntax_kind::name_expression:
    return bind_name_expression(
        dynamic_cast<const syntax::name_expression_syntax *>(syntax));
  case syntax::syntax_kind::parenthesized_expression:
    return bind_parenthesized_expression(
        dynamic_cast<const syntax::parenthesized_expression_syntax *>(syntax));
  case syntax::syntax_kind::unary_expression:
    return bind_unary_expression(
        dynamic_cast<const syntax::unary_expression_syntax *>(syntax));
  default:
    throw std::runtime_error{fmt::format(
        "Unexpected syntax {}", magic_enum::enum_name(syntax->kind()))};
  }
}

const minsk::analysis::diagnostic_bag &
minsk::analysis::binding::binder::diagnostics() const {
  return m_diagnostics;
}
