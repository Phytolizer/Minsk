#include "minsk/analysis/text/span.hpp"

minsk::analysis::text::text_span::text_span(int start, int length)
    : m_start(start), m_length(length) {}

minsk::analysis::text::text_span
minsk::analysis::text::text_span::from_bounds(int start, int end) {
  return text_span{start, end - start};
}

int minsk::analysis::text::text_span::start() const { return m_start; }

int minsk::analysis::text::text_span::length() const { return m_length; }

int minsk::analysis::text::text_span::end() const { return m_start + m_length; }

std::ostream &minsk::analysis::text::operator<<(
    std::ostream &os, const minsk::analysis::text::text_span &span) {
  return os << "text_span{start: " << span.start()
            << ", length: " << span.length() << "}";
}
