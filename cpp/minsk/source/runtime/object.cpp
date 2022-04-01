#include "minsk/runtime/object.hpp"
#include <stdexcept>

minsk::runtime::integer::integer(int value) : m_value(value) {}
int minsk::runtime::integer::value() const { return m_value; }
std::ostream &minsk::runtime::integer::print(std::ostream &os) const {
  return os << m_value;
}
bool minsk::runtime::integer::operator==(const object &other) const {
  return other.kind() == object_kind::integer &&
         other.as_integer()->value() == m_value;
}
minsk::runtime::object_kind minsk::runtime::integer::kind() const {
  return object_kind::integer;
}
minsk::runtime::boolean::boolean(bool value) : m_value(value) {}
bool minsk::runtime::boolean::value() const { return m_value; }
std::ostream &minsk::runtime::boolean::print(std::ostream &os) const {
  return os << (m_value ? "true" : "false");
}
bool minsk::runtime::boolean::operator==(const object &other) const {
  return other.kind() == object_kind::boolean &&
         other.as_boolean()->value() == m_value;
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
  case object_kind::null:
    return nullptr;
  case object_kind::integer:
    return std::make_unique<integer>(ptr->as_integer()->value());
  case object_kind::boolean:
    return std::make_unique<boolean>(ptr->as_boolean()->value());
  }

  throw std::runtime_error{"unreachable"};
}

std::ostream &minsk::runtime::operator<<(std::ostream &os, const object &obj) {
  return obj.print(os);
}

const minsk::runtime::boolean *minsk::runtime::object::as_boolean() const {
  return dynamic_cast<const boolean *>(this);
}

const minsk::runtime::integer *minsk::runtime::object::as_integer() const {
  return dynamic_cast<const integer *>(this);
}
