#include "minsk/analysis/binding/nodes/statements/variable.hpp"
#include "minsk/analysis/binding/kind.hpp"

minsk::analysis::binding::bound_variable_declaration::
    bound_variable_declaration(variable_symbol &&variable,
                               std::unique_ptr<bound_expression> initializer)
    : m_variable(std::move(variable)), m_initializer(std::move(initializer)) {}

minsk::analysis::binding::bound_node_kind
minsk::analysis::binding::bound_variable_declaration::kind() const {
  return bound_node_kind::variable_declaration;
}

const minsk::analysis::variable_symbol &
minsk::analysis::binding::bound_variable_declaration::variable() const {
  return m_variable;
}

const minsk::analysis::binding::bound_expression *
minsk::analysis::binding::bound_variable_declaration::initializer() const {
  return m_initializer.get();
}
