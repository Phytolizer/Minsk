#include "minsk/analysis/syntax/lexer.hpp"
#include "fmt/format.h"
#include "minsk/analysis/syntax/facts.hpp"
#include <cctype>
#include <optional>
#include <sstream>
minsk::analysis::syntax::lexer::iterator::iterator(
    minsk::analysis::syntax::lexer *lex)
    : m_lexer(lex), m_position(0), m_at_end(false), m_just_scanned(scan()) {}
minsk::analysis::syntax::lexer::iterator::iterator()
    : m_lexer(nullptr), m_position(0), m_at_end(true),
      m_just_scanned(syntax_kind::bad_token, 0, "", nullptr) {}
minsk::analysis::syntax::syntax_token
minsk::analysis::syntax::lexer::iterator::scan() {
  if (m_at_end) {
    m_lexer = nullptr;
    return syntax_token{
        syntax_kind::bad_token,
        m_position,
        "",
        nullptr,
    };
  }
  if (m_position >= m_lexer->m_text.length()) {
    m_at_end = true;
    return syntax_token{
        syntax_kind::end_of_file_token,
        m_position,
        "",
        nullptr,
    };
  }
  int start = m_position;
  auto kind = syntax_kind::bad_token;
  std::optional<std::string> text = std::nullopt;
  runtime::object_ptr value = nullptr;

  if (std::isdigit(current())) {
    while (std::isdigit(current())) {
      m_position += 1;
    }

    text = current_text(start);
    std::istringstream ss{*text};
    int int_value;
    if (!(ss >> int_value)) {
      m_lexer->m_diagnostics.emplace_back(
          fmt::format("Number {} doesn't fit in an int.", *text));
    }

    value = std::make_unique<runtime::integer>(int_value);
    kind = syntax_kind::number_token;
  } else if (std::isalpha(current())) {
    while (std::isalpha(current())) {
      m_position += 1;
    }

    text = current_text(start);
    kind = facts::keyword_kind(*text);
  } else if (std::isspace(current())) {
    while (std::isspace(current())) {
      m_position += 1;
    }

    kind = syntax_kind::whitespace_token;
  } else {
    switch (current()) {
    case '+':
      kind = syntax_kind::plus_token;
      m_position += 1;
      break;
    case '-':
      kind = syntax_kind::minus_token;
      m_position += 1;
      break;
    case '*':
      kind = syntax_kind::star_token;
      m_position += 1;
      break;
    case '/':
      kind = syntax_kind::slash_token;
      m_position += 1;
      break;
    case '(':
      kind = syntax_kind::open_parenthesis_token;
      m_position += 1;
      break;
    case ')':
      kind = syntax_kind::close_parenthesis_token;
      m_position += 1;
      break;
    }
  }

  if (kind == syntax_kind::bad_token) {
    m_lexer->m_diagnostics.emplace_back(
        fmt::format("Bad character in input: '{}'", current()));
    m_position += 1;
  }

  if (!text) {
    text = current_text(start);
  }

  return syntax_token{kind, start, *text, std::move(value)};
}
const minsk::analysis::syntax::syntax_token &
minsk::analysis::syntax::lexer::iterator::operator*() const {
  return m_just_scanned;
}
minsk::analysis::syntax::lexer::iterator &
minsk::analysis::syntax::lexer::iterator::operator++() {
  m_just_scanned = scan();
  return *this;
}
minsk::analysis::syntax::lexer::iterator
minsk::analysis::syntax::lexer::iterator::operator++(int) {
  iterator temp = *this;
  scan();
  return temp;
}
bool minsk::analysis::syntax::lexer::iterator::operator==(
    const minsk::analysis::syntax::lexer::iterator &other) const {
  if (m_lexer == nullptr) {
    return other.m_lexer == nullptr;
  }
  return m_lexer == other.m_lexer && m_position == other.m_position;
}
char minsk::analysis::syntax::lexer::iterator::current() const {
  return peek(0);
}
char minsk::analysis::syntax::lexer::iterator::peek(int offset) const {
  int index = m_position + offset;
  if (index >= m_lexer->m_text.length()) {
    return '\0';
  }
  return m_lexer->m_text[m_position];
}
std::string
minsk::analysis::syntax::lexer::iterator::current_text(int start) const {
  return std::string{
      m_lexer->m_text.begin() + start,
      m_lexer->m_text.begin() + m_position,
  };
}
minsk::analysis::syntax::lexer::iterator
minsk::analysis::syntax::lexer::begin() {
  return iterator{this};
}
minsk::analysis::syntax::lexer::iterator minsk::analysis::syntax::lexer::end() {
  return iterator{};
}
minsk::analysis::syntax::lexer::lexer(std::string_view text) : m_text(text) {}
const minsk::analysis::diagnostic_bag &
minsk::analysis::syntax::lexer::diagnostics() const {
  return m_diagnostics;
}
