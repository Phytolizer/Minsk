#ifndef MINSK_ANALYSIS_BINDING_NODES_STATEMENTS_VARIABLE_HPP
#define MINSK_ANALYSIS_BINDING_NODES_STATEMENTS_VARIABLE_HPP

#include "minsk/analysis/binding/kind.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/binding/nodes/statement.hpp"
#include "minsk/analysis/variable_symbol.hpp"
namespace minsk::analysis::binding {

class bound_variable_declaration final : public bound_statement {
  variable_symbol m_variable;
  std::unique_ptr<bound_expression> m_initializer;

public:
  bound_variable_declaration(variable_symbol &&variable,
                             std::unique_ptr<bound_expression> initializer);

  bound_node_kind kind() const override;

  const variable_symbol &variable() const;
  const bound_expression *initializer() const;
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_NODES_STATEMENTS_VARIABLE_HPP
