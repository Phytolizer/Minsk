#include "minsk/runtime/object.hpp"

minsk::runtime::integer::integer(int value) : m_value(value) {}
int minsk::runtime::integer::value() const { return m_value; }
minsk::runtime::boolean::boolean(bool value) : m_value(value) {}
bool minsk::runtime::boolean::value() const { return m_value; }
