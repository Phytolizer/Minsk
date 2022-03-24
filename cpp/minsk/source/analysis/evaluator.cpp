#include "minsk/analysis/evaluator.hpp"
#include "fmt/format.h"
#include "minsk/analysis/syntax/nodes/expressions/binary.hpp"
#include "minsk/analysis/syntax/nodes/expressions/literal.hpp"
#include "minsk/analysis/syntax/nodes/expressions/parenthesized.hpp"
#include "minsk/analysis/syntax/nodes/expressions/unary.hpp"
#include <stdexcept>
minsk::analysis::evaluator::evaluator(
    const minsk::analysis::syntax::expression_syntax *root)
    : m_root(root) {}
int minsk::analysis::evaluator::evaluate_expression(
    const minsk::analysis::syntax::expression_syntax *root) const {
  switch (root->kind()) {
  case syntax::syntax_kind::literal_expression:
    return evaluate_literal_expression(
        dynamic_cast<const syntax::literal_expression_syntax *>(root));
  case syntax::syntax_kind::binary_expression:
    return evaluate_binary_expression(
        dynamic_cast<const syntax::binary_expression_syntax *>(root));
  case syntax::syntax_kind::unary_expression:
    return evaluate_unary_expression(
        dynamic_cast<const syntax::unary_expression_syntax *>(root));
  case syntax::syntax_kind::parenthesized_expression:
    return evaluate_parenthesized_expression(
        dynamic_cast<const syntax::parenthesized_expression_syntax *>(root));
  default:
    throw std::runtime_error{fmt::format("unexpected syntax {}",
                                         magic_enum::enum_name(root->kind()))};
  }
}
int minsk::analysis::evaluator::evaluate() const {
  return evaluate_expression(m_root);
}
int minsk::analysis::evaluator::evaluate_binary_expression(
    const minsk::analysis::syntax::binary_expression_syntax *root) const {
  int left = evaluate_expression(root->left());
  int right = evaluate_expression(root->right());
  switch (root->operator_token().kind()) {
  case syntax::syntax_kind::plus_token:
    return left + right;
  case syntax::syntax_kind::minus_token:
    return left - right;
  case syntax::syntax_kind::star_token:
    return left * right;
  case syntax::syntax_kind::slash_token:
    return left / right;
  default:
    throw std::runtime_error{
        fmt::format("unknown binary operator {}",
                    magic_enum::enum_name(root->operator_token().kind()))};
  }
}
int minsk::analysis::evaluator::evaluate_literal_expression(
    const minsk::analysis::syntax::literal_expression_syntax *root) const {
  return dynamic_cast<const runtime::integer *>(root->literal_token().value())
      ->value();
}
int minsk::analysis::evaluator::evaluate_parenthesized_expression(
    const minsk::analysis::syntax::parenthesized_expression_syntax *root)
    const {
  return evaluate_expression(root->expression());
}
int minsk::analysis::evaluator::evaluate_unary_expression(
    const minsk::analysis::syntax::unary_expression_syntax *root) const {
  int operand = evaluate_expression(root->operand());
  switch (root->operator_token().kind()) {
  case syntax::syntax_kind::plus_token:
    return operand;
  case syntax::syntax_kind::minus_token:
    return -operand;
  default:
    throw std::runtime_error{
        fmt::format("unknown unary operator {}",
                    magic_enum::enum_name(root->operator_token().kind()))};
  }
}
