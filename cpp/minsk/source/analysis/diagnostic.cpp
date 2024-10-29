#include "minsk/analysis/diagnostic.hpp"

using minsk::analysis::diagnostic;
using minsk::analysis::text::text_span;

diagnostic::diagnostic(text::text_span span, std::string message)
    : m_span(span), m_message(std::move(message)) {}

const text_span &diagnostic::span() const { return m_span; }

std::string_view diagnostic::message() const { return m_message; }

std::ostream &operator<<(std::ostream &os, const diagnostic &diagnostic) {
  return os << diagnostic.message();
}
