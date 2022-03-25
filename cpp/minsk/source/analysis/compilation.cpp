#include "minsk/analysis/compilation.hpp"
#include "minsk/analysis/binding/binder.hpp"
#include "minsk/analysis/diagnostic_bag.hpp"
#include "minsk/analysis/evaluation_result.hpp"
#include "minsk/analysis/evaluator.hpp"
#include "minsk/analysis/variable_map.hpp"
#include <algorithm>
#include <iterator>

minsk::analysis::compilation::compilation(syntax::syntax_tree syntax)
    : m_syntax(std::move(syntax)) {}

const minsk::analysis::syntax::syntax_tree &
minsk::analysis::compilation::syntax() const {
  return m_syntax;
}

minsk::analysis::evaluation_result minsk::analysis::compilation::evaluate(
    minsk::analysis::variable_map *variables) const {
  binding::binder binder{variables};
  auto expression = binder.bind_expression(m_syntax.root()->expression());
  diagnostic_bag diagnostics;
  std::copy(m_syntax.diagnostics().begin(), m_syntax.diagnostics().end(),
            std::back_inserter(diagnostics));
  std::copy(binder.diagnostics().begin(), binder.diagnostics().end(),
            std::back_inserter(diagnostics));
  if (!diagnostics.empty()) {
    return evaluation_result{std::move(diagnostics)};
  }
  return evaluation_result{evaluator{expression.get(), variables}.evaluate()};
}
