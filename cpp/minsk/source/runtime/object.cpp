#include "minsk/runtime/object.hpp"
#include <stdexcept>

minsk::runtime::integer::integer(int value) : m_value(value) {}
int minsk::runtime::integer::value() const { return m_value; }
std::ostream &minsk::runtime::integer::print(std::ostream &os) const {
  return os << m_value;
}
minsk::runtime::object_kind minsk::runtime::integer::kind() const {
  return object_kind::integer;
}
minsk::runtime::boolean::boolean(bool value) : m_value(value) {}
bool minsk::runtime::boolean::value() const { return m_value; }
std::ostream &minsk::runtime::boolean::print(std::ostream &os) const {
  return os << (m_value ? "true" : "false");
}
minsk::runtime::object_kind minsk::runtime::boolean::kind() const {
  return object_kind::boolean;
}
std::unique_ptr<minsk::runtime::object>
minsk::runtime::copy_object_ptr(const minsk::runtime::object *ptr) {
  if (ptr == nullptr) {
    return nullptr;
  }

  switch (ptr->kind()) {
  case object_kind::integer:
    return std::make_unique<integer>(
        dynamic_cast<const integer *>(ptr)->value());
  case object_kind::boolean:
    return std::make_unique<boolean>(
        dynamic_cast<const boolean *>(ptr)->value());
  }

  throw std::runtime_error{"unreachable"};
}
