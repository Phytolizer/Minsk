#ifndef MINSK_ANALYSIS_BINDING_SCOPE_GLOBAL_HPP
#define MINSK_ANALYSIS_BINDING_SCOPE_GLOBAL_HPP

#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/binding/nodes/statement.hpp"
#include "minsk/analysis/diagnostic.hpp"
#include "minsk/analysis/variable_symbol.hpp"
#include <memory>
#include <vector>
namespace minsk::analysis::binding {

class bound_global_scope final {
  bound_global_scope *m_previous;
  std::vector<diagnostic> m_diagnostics;
  std::vector<variable_symbol> m_variables;
  std::unique_ptr<bound_statement> m_statement;

public:
  bound_global_scope(bound_global_scope *previous,
                     std::vector<diagnostic> &&diagnostics,
                     std::vector<variable_symbol> &&variables,
                     std::unique_ptr<bound_statement> statement);

  bound_global_scope *previous() const;
  const std::vector<diagnostic> &diagnostics() const;
  const std::vector<variable_symbol> &variables() const;
  const bound_statement *statement() const;
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_SCOPE_GLOBAL_HPP
