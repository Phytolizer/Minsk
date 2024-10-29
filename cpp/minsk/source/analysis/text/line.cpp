#include "minsk/analysis/text/line.hpp"
#include "minsk/analysis/text/span.hpp"

using minsk::analysis::text::text_line;
using minsk::analysis::text::text_span;

text_line::text_line(int start, int length, int length_including_line_break)
    : m_start(start), m_length(length),
      m_length_including_line_break(length_including_line_break) {}

int text_line::start() const { return m_start; }

int text_line::length() const { return m_length; }

int text_line::length_including_line_break() const {
  return m_length_including_line_break;
}

int text_line::end() const { return m_start + m_length; }

int text_line::end_including_line_break() const {
  return m_start + m_length_including_line_break;
}

text_span text_line::span() const { return text_span{m_start, m_length}; }

text_span text_line::span_including_line_break() const {
  return text_span{m_start, m_length_including_line_break};
}
