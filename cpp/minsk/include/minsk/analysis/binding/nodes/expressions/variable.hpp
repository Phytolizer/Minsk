#ifndef MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_NAME_HPP
#define MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_NAME_HPP

#include "minsk/analysis/binding/kind.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/runtime/object.hpp"
#include <string>
#include <string_view>
namespace minsk::analysis::binding {

class bound_variable_expression final : public bound_expression {
  std::string m_name;
  runtime::object_kind m_type;

  public:
  bound_variable_expression(std::string &&name, runtime::object_kind type);

  bound_node_kind kind() const override;
  runtime::object_kind type() const override;

  std::string_view name() const;
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_NAME_HPP
