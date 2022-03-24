#ifndef MINSK_EVALUATOR_HPP_D4C6D6F8D25B4A42A184376F00B7A83C
#define MINSK_EVALUATOR_HPP_D4C6D6F8D25B4A42A184376F00B7A83C

#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/binding/nodes/expressions/binary.hpp"
#include "minsk/analysis/binding/nodes/expressions/literal.hpp"
#include "minsk/analysis/binding/nodes/expressions/unary.hpp"
#include <memory>

namespace minsk::analysis {

class evaluator final {
  const binding::bound_expression *m_root;

  int evaluate_expression(const binding::bound_expression *root) const;
  int evaluate_binary_expression(
      const binding::bound_binary_expression *root) const;
  int evaluate_literal_expression(
      const binding::bound_literal_expression *root) const;
  int evaluate_unary_expression(
      const binding::bound_unary_expression *root) const;

public:
  explicit evaluator(const binding::bound_expression *root);
  int evaluate() const;
};

} // namespace minsk::analysis

#endif // MINSK_EVALUATOR_HPP_D4C6D6F8D25B4A42A184376F00B7A83C
