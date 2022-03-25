#ifndef MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_ASSIGNMENT_HPP
#define MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_ASSIGNMENT_HPP

#include "minsk/analysis/binding/kind.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/runtime/object.hpp"
#include <memory>
#include <string>

namespace minsk::analysis::binding {

class bound_assignment_expression final : public bound_expression {
  std::string m_name;
  std::unique_ptr<bound_expression> m_expression;

public:
  bound_assignment_expression(std::string &&name,
                              std::unique_ptr<bound_expression> expression);

  bound_node_kind kind() const override;
  runtime::object_kind type() const override;

  std::string_view name() const;
  const bound_expression *expression() const;
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_NODES_EXPRESSIONS_ASSIGNMENT_HPP
