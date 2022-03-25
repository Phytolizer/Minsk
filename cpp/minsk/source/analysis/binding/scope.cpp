#include "minsk/analysis/binding/scope.hpp"
#include "minsk/analysis/variable_symbol.hpp"
#include <algorithm>
#include <iterator>
#include <ranges>

minsk::analysis::binding::bound_scope::bound_scope(bound_scope *previous)
    : m_parent(previous) {}

minsk::analysis::binding::bound_scope *
minsk::analysis::binding::bound_scope::parent() {
  return m_parent;
}

std::vector<minsk::analysis::variable_symbol>
minsk::analysis::binding::bound_scope::get_declared_variables() const {
  std::vector<variable_symbol> declared_variables;
  std::ranges::transform(m_variables, std::back_inserter(declared_variables),
                         [](const auto &pair) { return pair.second; });
  return declared_variables;
}

bool minsk::analysis::binding::bound_scope::try_declare(
    variable_symbol &&variable) {
  if (m_variables.find(std::string{variable.name()}) != m_variables.end()) {
    return false;
  }

  m_variables.emplace(std::string{variable.name()}, variable);
  return true;
}

std::optional<minsk::analysis::variable_symbol>
minsk::analysis::binding::bound_scope::try_lookup(std::string_view name) {
  auto it = m_variables.find(std::string{name});
  if (it != m_variables.end()) {
    return it->second;
  }

  if (m_parent == nullptr) {
    return {};
  }

  return m_parent->try_lookup(name);
}
