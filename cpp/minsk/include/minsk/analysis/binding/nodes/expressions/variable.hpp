#ifndef MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_NAME_HPP
#define MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_NAME_HPP

#include "minsk/analysis/binding/kind.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/variable_symbol.hpp"
#include "minsk/runtime/object.hpp"
#include <string>
#include <string_view>
namespace minsk::analysis::binding {

class bound_variable_expression final : public bound_expression {
  variable_symbol m_variable;

  public:
  explicit bound_variable_expression(variable_symbol &&variable);

  bound_node_kind kind() const override;
  runtime::object_kind type() const override;

  const variable_symbol &variable() const;
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_NAME_HPP
