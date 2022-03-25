#ifndef MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_ASSIGNMENT_HPP
#define MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_ASSIGNMENT_HPP

#include "minsk/analysis/binding/kind.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/variable_symbol.hpp"
#include "minsk/runtime/object.hpp"
#include <memory>
#include <string>

namespace minsk::analysis::binding {

class bound_assignment_expression final : public bound_expression {
  variable_symbol m_variable;
  std::unique_ptr<bound_expression> m_expression;

public:
  bound_assignment_expression(variable_symbol &&variable,
                              std::unique_ptr<bound_expression> expression);

  bound_node_kind kind() const override;
  runtime::object_kind type() const override;

  const variable_symbol &variable() const;
  const bound_expression *expression() const;
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_ASSIGNMENT_HPP
