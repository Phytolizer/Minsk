#include "minsk/analysis/variable_symbol.hpp"
#include "minsk/runtime/object.hpp"

using minsk::analysis::variable_symbol;
using minsk::runtime::object_kind;

variable_symbol::variable_symbol(std::string &&name, bool is_read_only,
                                 runtime::object_kind type)
    : m_name(std::move(name)), m_is_read_only(is_read_only), m_type(type) {}

std::string_view variable_symbol::name() const { return m_name; }

bool variable_symbol::is_read_only() const { return m_is_read_only; }

object_kind variable_symbol::type() const { return m_type; }

bool variable_symbol::operator==(const variable_symbol &other) const {
  return m_name == other.m_name && m_type == other.m_type;
}
