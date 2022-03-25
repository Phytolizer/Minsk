#ifndef MINSK_ANALYSIS_BINDING_SCOPE_HPP
#define MINSK_ANALYSIS_BINDING_SCOPE_HPP

#include "minsk/analysis/variable_symbol.hpp"
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>
namespace minsk::analysis::binding {

class bound_scope final {
  bound_scope *m_parent;
  std::unordered_map<std::string, variable_symbol> m_variables;

public:
  explicit bound_scope(bound_scope *previous);
  bound_scope *parent();
  std::vector<variable_symbol> get_declared_variables() const;
  bool try_declare(variable_symbol &&variable);
  std::optional<variable_symbol> try_lookup(std::string_view name);
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_SCOPE_HPP
