#ifndef MINSK_ANALYSIS_BINDING_SCOPE_HPP
#define MINSK_ANALYSIS_BINDING_SCOPE_HPP

#include "minsk/analysis/variable_symbol.hpp"
#include <memory>
#include <optional>
#include <string>
#include <string_view>
#include <unordered_map>
#include <vector>
namespace minsk::analysis::binding {

class bound_scope final {
  std::unique_ptr<bound_scope> m_parent;
  std::unordered_map<std::string, variable_symbol> m_variables;

public:
  explicit bound_scope(std::unique_ptr<bound_scope> parent);
  const bound_scope *parent() const;
  std::unique_ptr<bound_scope> take_parent();
  std::vector<variable_symbol> get_declared_variables() const;
  bool try_declare(variable_symbol &&variable);
  std::optional<variable_symbol> try_lookup(std::string_view name);
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_SCOPE_HPP
