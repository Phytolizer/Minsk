#ifndef MINSK_ANALYSIS_TEXT_SPAN_HPP
#define MINSK_ANALYSIS_TEXT_SPAN_HPP

namespace minsk::analysis::text {

class text_span final {
  int m_start;
  int m_length;

public:
  text_span(int start, int length);
  static text_span from_bounds(int start, int end);
  int start() const;
  int length() const;
  int end() const;
};

} // namespace minsk::analysis::text

#endif // MINSK_ANALYSIS_TEXT_SPAN_HPP
