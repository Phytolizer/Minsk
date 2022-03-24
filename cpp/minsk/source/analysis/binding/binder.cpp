#include "minsk/analysis/binding/binder.hpp"
#include "fmt/core.h"
#include "magic_enum.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/binding/nodes/expressions/binary.hpp"
#include "minsk/analysis/binding/nodes/expressions/binary/operator.hpp"
#include "minsk/analysis/binding/nodes/expressions/literal.hpp"
#include "minsk/analysis/binding/nodes/expressions/unary.hpp"
#include "minsk/analysis/binding/nodes/expressions/unary/operator.hpp"
#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/nodes/expressions/binary.hpp"
#include "minsk/analysis/syntax/nodes/expressions/literal.hpp"
#include "minsk/analysis/syntax/nodes/expressions/parenthesized.hpp"
#include "minsk/analysis/syntax/nodes/expressions/unary.hpp"
#include "minsk/runtime/object.hpp"
#include <memory>
#include <stdexcept>

std::unique_ptr<minsk::analysis::binding::bound_expression>
minsk::analysis::binding::binder::bind_binary_expression(
    const syntax::binary_expression_syntax *syntax) {
  std::unique_ptr<bound_expression> left = bind_expression(syntax->left());
  std::unique_ptr<bound_expression> right = bind_expression(syntax->right());

  const bound_binary_operator *op = bound_binary_operator::bind(
      syntax->operator_token().kind(), left->type(), right->type());
  if (op == nullptr) {
    m_diagnostics.emplace_back(fmt::format(
        "Binary operator '{}' isn't defined for types {} and {}",
        syntax->operator_token().text(), magic_enum::enum_name(left->type()),
        magic_enum::enum_name(right->type())));
    return left;
  }

  return std::make_unique<bound_binary_expression>(std::move(left), op,
                                                   std::move(right));
}

std::unique_ptr<minsk::analysis::binding::bound_expression>
minsk::analysis::binding::binder::bind_literal_expression(
    const syntax::literal_expression_syntax *syntax) {
  return std::make_unique<bound_literal_expression>(
      runtime::copy_object_ptr(syntax->literal_token().value()));
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
    m_diagnostics.emplace_back(
        fmt::format("Unary operator '{}' isn't defined for type {}",
                    syntax->operator_token().text(),
                    magic_enum::enum_name(operand->type())));
    return operand;
  }

  return std::make_unique<bound_unary_expression>(op, std::move(operand));
}

std::unique_ptr<minsk::analysis::binding::bound_expression>
minsk::analysis::binding::binder::bind_expression(
    const syntax::expression_syntax *syntax) {
  switch (syntax->kind()) {
  case syntax::syntax_kind::binary_expression:
    return bind_binary_expression(
        dynamic_cast<const syntax::binary_expression_syntax *>(syntax));
  case syntax::syntax_kind::literal_expression:
    return bind_literal_expression(
        dynamic_cast<const syntax::literal_expression_syntax *>(syntax));
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
