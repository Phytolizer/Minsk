#include "minsk/analysis/binding/scope/global.hpp"
#include "minsk/analysis/diagnostic.hpp"

minsk::analysis::binding::bound_global_scope::bound_global_scope(
    bound_global_scope *previous, std::vector<diagnostic> &&diagnostics,
    std::vector<variable_symbol> &&variables,
    std::unique_ptr<bound_statement> statement)
    : m_previous(previous), m_diagnostics(std::move(diagnostics)),
      m_variables(std::move(variables)), m_statement(std::move(statement)) {}

minsk::analysis::binding::bound_global_scope *
minsk::analysis::binding::bound_global_scope::previous() const {
  return m_previous;
}

const std::vector<minsk::analysis::diagnostic> &
minsk::analysis::binding::bound_global_scope::diagnostics() const {
  return m_diagnostics;
}

const std::vector<minsk::analysis::variable_symbol> &
minsk::analysis::binding::bound_global_scope::variables() const {
  return m_variables;
}

const minsk::analysis::binding::bound_statement *
minsk::analysis::binding::bound_global_scope::statement() const {
  return m_statement.get();
}
