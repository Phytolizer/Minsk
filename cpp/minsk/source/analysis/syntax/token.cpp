#include "minsk/analysis/syntax/token.hpp"
#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/text/span.hpp"
#include <utility>

using minsk::analysis::syntax::syntax_kind;
using minsk::analysis::syntax::syntax_node;
using minsk::analysis::syntax::syntax_token;
using minsk::analysis::text::text_span;
using minsk::runtime::copy_object_ptr;
using minsk::runtime::object;
using minsk::runtime::object_ptr;

syntax_token::syntax_token(syntax_kind kind, int position, std::string text,
                           object_ptr value)
    : m_kind(kind), m_position(position), m_text(text),
      m_value(std::move(value)) {}

syntax_kind syntax_token::kind() const { return m_kind; }

int syntax_token::position() const { return m_position; }

std::string_view syntax_token::text() const { return m_text; }

const object *syntax_token::value() const { return m_value.get(); }

text_span syntax_token::span() const {
  return text_span{m_position, static_cast<int>(m_text.length())};
}

syntax_token::syntax_token(const syntax_token &other)
    : m_kind(other.m_kind), m_position(other.m_position), m_text(other.m_text),
      m_value(copy_object_ptr(other.m_value.get())) {}

syntax_token &syntax_token::operator=(const syntax_token &other) {
  if (&other != this) {
    m_kind = other.m_kind;
    m_position = other.m_position;
    m_text = other.m_text;
    if (other.m_value) {
      m_value = copy_object_ptr(other.m_value.get());
    } else {
      m_value = nullptr;
    }
  }
  return *this;
}

std::vector<const syntax_node *> syntax_token::children() const { return {}; }
