#ifndef MINSK_ANALYSIS_BINDING_NODES_STATEMENTS_FOR_HPP
#define MINSK_ANALYSIS_BINDING_NODES_STATEMENTS_FOR_HPP

#include "minsk/analysis/binding/kind.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/binding/nodes/statement.hpp"
#include "minsk/analysis/variable_symbol.hpp"
#include <memory>
namespace minsk::analysis::binding {

class bound_for_statement final : public bound_statement {
  variable_symbol m_variable;
  std::unique_ptr<bound_expression> m_initial_value;
  std::unique_ptr<bound_expression> m_final_value;
  std::unique_ptr<bound_statement> m_body;

public:
  bound_for_statement(variable_symbol &&variable,
                      std::unique_ptr<bound_expression> initial_value,
                      std::unique_ptr<bound_expression> final_value,
                      std::unique_ptr<bound_statement> body);

  bound_node_kind kind() const override;

  const variable_symbol &variable() const;
  const bound_expression *initial_value() const;
  const bound_expression *final_value() const;
  const bound_statement *body() const;
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_NODES_STATEMENTS_FOR_HPP
