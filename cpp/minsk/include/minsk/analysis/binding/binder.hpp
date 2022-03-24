#ifndef MINSK_ANALYSIS_BINDING_BINDER_HPP
#define MINSK_ANALYSIS_BINDING_BINDER_HPP

#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/diagnostic_bag.hpp"
#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/nodes/expressions/binary.hpp"
#include "minsk/analysis/syntax/nodes/expressions/literal.hpp"
#include "minsk/analysis/syntax/nodes/expressions/parenthesized.hpp"
#include "minsk/analysis/syntax/nodes/expressions/unary.hpp"
#include <memory>

namespace minsk::analysis::binding {

class binder final {
  diagnostic_bag m_diagnostics;
  std::unique_ptr<bound_expression>
  bind_binary_expression(const syntax::binary_expression_syntax *syntax);
  std::unique_ptr<bound_expression>
  bind_literal_expression(const syntax::literal_expression_syntax *syntax);
  std::unique_ptr<bound_expression> bind_parenthesized_expression(
      const syntax::parenthesized_expression_syntax *syntax);
  std::unique_ptr<bound_expression>
  bind_unary_expression(const syntax::unary_expression_syntax *syntax);

public:
  binder() = default;
  std::unique_ptr<bound_expression>
  bind_expression(const syntax::expression_syntax *syntax);
  const diagnostic_bag &diagnostics() const;
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_BINDER_HPP
