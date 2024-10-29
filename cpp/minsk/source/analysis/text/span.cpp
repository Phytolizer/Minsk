#include "minsk/analysis/text/span.hpp"

using minsk::analysis::text::text_span;
namespace text = minsk::analysis::text;

text_span::text_span(int start, int length)
    : m_start(start), m_length(length) {}

text_span text_span::from_bounds(int start, int end) {
  return text_span{start, end - start};
}

int text_span::start() const { return m_start; }

int text_span::length() const { return m_length; }

int text_span::end() const { return m_start + m_length; }

std::ostream &text::operator<<(std::ostream &os, const text_span &span) {
  return os << "text_span{start: " << span.start()
            << ", length: " << span.length() << "}";
}
