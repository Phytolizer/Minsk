#include "minsk/analysis/diagnostic.hpp"

minsk::analysis::diagnostic::diagnostic(text::text_span span,
                                        std::string message)
    : m_span(span), m_message(std::move(message)) {}

const minsk::analysis::text::text_span &
minsk::analysis::diagnostic::span() const {
  return m_span;
}

std::string_view minsk::analysis::diagnostic::message() const {
  return m_message;
}

std::ostream &operator<<(std::ostream &os,
                         const minsk::analysis::diagnostic &diagnostic) {
  return os << diagnostic.message();
}
