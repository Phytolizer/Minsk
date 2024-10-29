#include "minsk/runtime/object.hpp"
#include <stdexcept>

using minsk::runtime::boolean;
using minsk::runtime::integer;
using minsk::runtime::object;
using minsk::runtime::object_kind;
namespace runtime = minsk::runtime;

integer::integer(int value) : m_value(value) {}

int integer::value() const { return m_value; }

std::ostream &integer::print(std::ostream &os) const { return os << m_value; }

bool integer::operator==(const object &other) const {
  return other.kind() == object_kind::integer &&
         other.as_integer()->value() == m_value;
}

object_kind integer::kind() const { return object_kind::integer; }

boolean::boolean(bool value) : m_value(value) {}

bool boolean::value() const { return m_value; }

std::ostream &boolean::print(std::ostream &os) const {
  return os << (m_value ? "true" : "false");
}

bool boolean::operator==(const object &other) const {
  return other.kind() == object_kind::boolean &&
         other.as_boolean()->value() == m_value;
}

object_kind boolean::kind() const { return object_kind::boolean; }

std::unique_ptr<object> runtime::copy_object_ptr(const object *ptr) {
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

std::ostream &runtime::operator<<(std::ostream &os, const object &obj) {
  return obj.print(os);
}

const boolean *object::as_boolean() const {
  return dynamic_cast<const boolean *>(this);
}

const integer *object::as_integer() const {
  return dynamic_cast<const integer *>(this);
}
