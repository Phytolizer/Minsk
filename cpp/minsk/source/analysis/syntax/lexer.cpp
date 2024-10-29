#include "minsk/analysis/syntax/lexer.hpp"
#include "minsk/analysis/syntax/facts.hpp"
#include "minsk/analysis/text/source.hpp"
#include "minsk/analysis/text/span.hpp"
#include <cctype>
#include <optional>
#include <sstream>

using minsk::analysis::syntax::lexer;
using minsk::analysis::syntax::syntax_token;

lexer::iterator::iterator(lexer *lex)
    : m_lexer(lex), m_position(0), m_at_end(false), m_just_scanned(scan()) {}

lexer::iterator::iterator()
    : m_lexer(nullptr), m_position(0), m_at_end(true),
      m_just_scanned(syntax_kind::bad_token, 0, "", nullptr) {}

syntax_token lexer::iterator::scan() {
  if (m_at_end) {
    m_lexer = nullptr;
    return syntax_token{
        syntax_kind::bad_token,
        m_position,
        "",
        nullptr,
    };
  }
  if (m_position >= m_lexer->m_text->length()) {
    m_at_end = true;
    return syntax_token{
        syntax_kind::end_of_file_token,
        m_lexer->m_text->length(),
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
      m_lexer->m_diagnostics.report_invalid_int(
          text::text_span::from_bounds(start, m_position), *text);
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
    case '~':
      kind = syntax_kind::tilde_token;
      m_position += 1;
      break;
    case '^':
      kind = syntax_kind::hat_token;
      m_position += 1;
      break;
    case '=':
      if (peek(1) == '=') {
        kind = syntax_kind::equals_equals_token;
        m_position += 2;
      } else {
        kind = syntax_kind::equals_token;
        m_position += 1;
      }
      break;
    case '!':
      if (peek(1) == '=') {
        kind = syntax_kind::bang_equals_token;
        m_position += 2;
      } else {
        kind = syntax_kind::bang_token;
        m_position += 1;
      }
      break;
    case '<':
      if (peek(1) == '=') {
        kind = syntax_kind::less_equals_token;
        m_position += 2;
      } else {
        kind = syntax_kind::less_token;
        m_position += 1;
      }
      break;
    case '>':
      if (peek(1) == '=') {
        kind = syntax_kind::greater_equals_token;
        m_position += 2;
      } else {
        kind = syntax_kind::greater_token;
        m_position += 1;
      }
      break;
    case '&':
      if (peek(1) == '&') {
        kind = syntax_kind::ampersand_ampersand_token;
        m_position += 2;
      } else {
        kind = syntax_kind::ampersand_token;
        m_position += 1;
      }
      break;
    case '|':
      if (peek(1) == '|') {
        kind = syntax_kind::pipe_pipe_token;
        m_position += 2;
      } else {
        kind = syntax_kind::pipe_token;
        m_position += 1;
      }
      break;
    case '(':
      kind = syntax_kind::open_parenthesis_token;
      m_position += 1;
      break;
    case ')':
      kind = syntax_kind::close_parenthesis_token;
      m_position += 1;
      break;
    case '{':
      kind = syntax_kind::open_brace_token;
      m_position += 1;
      break;
    case '}':
      kind = syntax_kind::close_brace_token;
      m_position += 1;
      break;
    default:
      break;
    }
  }

  if (kind == syntax_kind::bad_token) {
    m_lexer->m_diagnostics.report_bad_character(m_position, current());
    m_position += 1;
  }

  if (!text) {
    text = current_text(start);
  }

  return syntax_token{kind, start, *text, std::move(value)};
}

const syntax_token &lexer::iterator::operator*() const {
  return m_just_scanned;
}

lexer::iterator &lexer::iterator::operator++() {
  m_just_scanned = scan();
  return *this;
}

lexer::iterator lexer::iterator::operator++(int) {
  iterator temp = *this;
  scan();
  return temp;
}

bool lexer::iterator::operator==(const lexer::iterator &other) const {
  if (m_lexer == nullptr) {
    return other.m_lexer == nullptr;
  }
  return m_lexer == other.m_lexer && m_position == other.m_position;
}

char lexer::iterator::current() const { return peek(0); }

char lexer::iterator::peek(int offset) const {
  int index = m_position + offset;
  if (index >= m_lexer->m_text->length()) {
    return '\0';
  }
  return (*m_lexer->m_text)[index];
}

std::string lexer::iterator::current_text(int start) const {
  return m_lexer->m_text->to_string(start, m_position - start);
}

lexer::iterator lexer::begin() { return iterator{this}; }

lexer::iterator lexer::end() { return iterator{}; }

lexer::lexer(const text::source_text *text) : m_text(text) {}

minsk::analysis::diagnostic_bag &lexer::diagnostics() { return m_diagnostics; }
