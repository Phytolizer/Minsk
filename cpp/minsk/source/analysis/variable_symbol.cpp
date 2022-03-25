#include "minsk/analysis/variable_symbol.hpp"
#include "minsk/runtime/object.hpp"

minsk::analysis::variable_symbol::variable_symbol(std::string &&name,
                                                  runtime::object_kind type)
    : m_name(std::move(name)), m_type(type) {}

std::string_view minsk::analysis::variable_symbol::name() const {
  return m_name;
}

minsk::runtime::object_kind minsk::analysis::variable_symbol::type() const {
  return m_type;
}

bool minsk::analysis::variable_symbol::operator==(
    const variable_symbol &other) const {
  return m_name == other.m_name && m_type == other.m_type;
}
