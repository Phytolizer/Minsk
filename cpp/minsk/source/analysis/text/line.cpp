#include "minsk/analysis/text/line.hpp"
#include "minsk/analysis/text/span.hpp"

minsk::analysis::text::text_line::text_line(int start, int length,
                                            int length_including_line_break)
    : m_start(start), m_length(length),
      m_length_including_line_break(length_including_line_break) {}

int minsk::analysis::text::text_line::start() const { return m_start; }

int minsk::analysis::text::text_line::length() const { return m_length; }

int minsk::analysis::text::text_line::length_including_line_break() const {
  return m_length_including_line_break;
}

int minsk::analysis::text::text_line::end() const { return m_start + m_length; }

int minsk::analysis::text::text_line::end_including_line_break() const {
  return m_start + m_length_including_line_break;
}

minsk::analysis::text::text_span
minsk::analysis::text::text_line::span() const {
  return text_span{m_start, m_length};
}

minsk::analysis::text::text_span
minsk::analysis::text::text_line::span_including_line_break() const {
  return text_span{m_start, m_length_including_line_break};
}
