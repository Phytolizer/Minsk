#ifndef MINSK_ANALYSIS_TEXT_LINE_HPP
#define MINSK_ANALYSIS_TEXT_LINE_HPP

#include "span.hpp"

namespace minsk::analysis::text {

class text_line final {
  int m_start;
  int m_length;
  int m_length_including_line_break;

public:
  text_line(int start, int length, int length_including_line_break);

  int start() const;
  int length() const;
  int length_including_line_break() const;
  int end() const;
  int end_including_line_break() const;
  text_span span() const;
  text_span span_including_line_break() const;
};

} // namespace minsk::analysis::text

#endif // MINSK_ANALYSIS_TEXT_LINE_HPP
