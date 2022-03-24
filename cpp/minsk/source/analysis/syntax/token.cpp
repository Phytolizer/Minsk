#include "minsk/analysis/syntax/token.hpp"
minsk::analysis::syntax::syntax_token::syntax_token(
    minsk::analysis::syntax::syntax_kind kind, int position, std::string text,
    minsk::runtime::object_ptr value)
    : m_kind(kind), m_position(position), m_text(text),
      m_value(std::move(value)) {}
minsk::analysis::syntax::syntax_kind
minsk::analysis::syntax::syntax_token::kind() const {
  return m_kind;
}
int minsk::analysis::syntax::syntax_token::position() const {
  return m_position;
}
std::string_view minsk::analysis::syntax::syntax_token::text() const {
  return m_text;
}
minsk::runtime::object *minsk::analysis::syntax::syntax_token::value() const {
  return m_value.get();
}
minsk::analysis::syntax::syntax_token::syntax_token(
    const minsk::analysis::syntax::syntax_token &other)
    : m_kind(other.m_kind), m_position(other.m_position), m_text(other.m_text),
      m_value(other.m_value ? std::make_unique<runtime::object>(*other.m_value)
                            : nullptr) {}
minsk::analysis::syntax::syntax_token &
minsk::analysis::syntax::syntax_token::operator=(
    const minsk::analysis::syntax::syntax_token &other) {
  if (&other != this) {
    m_kind = other.m_kind;
    m_position = other.m_position;
    m_text = other.m_text;
    if (other.m_value) {
      m_value = std::make_unique<runtime::object>(*other.m_value);
    } else {
      m_value = nullptr;
    }
  }
  return *this;
}
