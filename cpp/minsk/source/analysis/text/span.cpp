#include "minsk/analysis/text/span.hpp"

minsk::analysis::text::text_span::text_span(int start, int length)
    : m_start(start), m_length(length) {}

minsk::analysis::text::text_span
minsk::analysis::text::text_span::from_bounds(int start, int end) {
  return text_span{start, end - start};
}

int minsk::analysis::text::text_span::end() const { return m_start + m_length; }
