#ifndef MINSK_EVALUATOR_HPP_D4C6D6F8D25B4A42A184376F00B7A83C
#define MINSK_EVALUATOR_HPP_D4C6D6F8D25B4A42A184376F00B7A83C

#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/nodes/expressions/binary.hpp"
#include "minsk/analysis/syntax/nodes/expressions/literal.hpp"
#include "minsk/analysis/syntax/nodes/expressions/parenthesized.hpp"
#include "minsk/analysis/syntax/nodes/expressions/unary.hpp"
#include <memory>

namespace minsk::analysis {

class evaluator final {
  const syntax::expression_syntax *m_root;

  int evaluate_expression(const syntax::expression_syntax *root) const;
  int evaluate_binary_expression(
      const syntax::binary_expression_syntax *root) const;
  int evaluate_literal_expression(
      const syntax::literal_expression_syntax *root) const;
  int evaluate_parenthesized_expression(
      const syntax::parenthesized_expression_syntax *root) const;
  int evaluate_unary_expression(
      const syntax::unary_expression_syntax *root) const;

public:
  explicit evaluator(const syntax::expression_syntax *root);
  int evaluate() const;
};

} // namespace minsk::analysis

#endif // MINSK_EVALUATOR_HPP_D4C6D6F8D25B4A42A184376F00B7A83C
