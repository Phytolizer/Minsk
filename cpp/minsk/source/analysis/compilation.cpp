#include "minsk/analysis/compilation.hpp"
#include "minsk/analysis/binding/binder.hpp"
#include "minsk/analysis/diagnostic_bag.hpp"
#include "minsk/analysis/evaluation_result.hpp"
#include "minsk/analysis/evaluator.hpp"
#include "minsk/analysis/variable_map.hpp"
#include <algorithm>
#include <iterator>
#include <memory>
#include <ranges>

minsk::analysis::compilation::compilation(std::unique_ptr<compilation> previous,
                                          syntax::syntax_tree syntax)
    : m_previous(std::move(previous)), m_syntax(std::move(syntax)),
      m_global_scope(binding::binder::bind_global_scope(
          m_previous ? &m_previous->m_global_scope : nullptr,
          m_syntax.root())) {}

minsk::analysis::compilation::compilation(syntax::syntax_tree syntax)
    : compilation(nullptr, std::move(syntax)) {}

const minsk::analysis::syntax::syntax_tree &
minsk::analysis::compilation::syntax() const {
  return m_syntax;
}

minsk::analysis::evaluation_result minsk::analysis::compilation::evaluate(
    minsk::analysis::variable_map *variables) const {
  diagnostic_bag diagnostics;
  std::ranges::copy(m_syntax.diagnostics(), std::back_inserter(diagnostics));
  std::ranges::copy(m_global_scope.diagnostics(),
                    std::back_inserter(diagnostics));
  if (!diagnostics.empty()) {
    return evaluation_result{std::move(diagnostics)};
  }
  auto evaluator = analysis::evaluator{m_global_scope.statement(), variables};
  return evaluation_result{evaluator.evaluate()};
}
std::unique_ptr<minsk::analysis::compilation> minsk::analysis::compilation::take_previous() {
  return std::move(m_previous);
}
