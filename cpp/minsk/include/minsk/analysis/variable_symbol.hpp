#ifndef MINSK_ANALYSIS_VARIABLE_SYMBOL_HPP
#define MINSK_ANALYSIS_VARIABLE_SYMBOL_HPP

#include "minsk/runtime/object.hpp"
#include <cstddef>
#include <string>
#include <string_view>
namespace minsk::analysis {

class variable_symbol final {
  std::string m_name;
  runtime::object_kind m_type;

public:
  variable_symbol(std::string &&name, runtime::object_kind type);

  std::string_view name() const;
  runtime::object_kind type() const;

  bool operator==(const variable_symbol &other) const;
};

} // namespace minsk::analysis

namespace std {

template <> struct hash<minsk::analysis::variable_symbol> {
  std::size_t
  operator()(const minsk::analysis::variable_symbol &variable) const {
    return hash<std::string_view>{}(variable.name());
  }
};

} // namespace std

#endif // MINSK_ANALYSIS_VARIABLE_SYMBOL_HPP
