#ifndef MINSK_ANALYSIS_TEXT_SPAN_HPP
#define MINSK_ANALYSIS_TEXT_SPAN_HPP

#include <ostream>
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

  constexpr bool operator==(const text_span &other) const {
    return m_start == other.m_start && m_length == other.m_length;
  }
};

std::ostream &operator<<(std::ostream &os,
                         const minsk::analysis::text::text_span &span);

} // namespace minsk::analysis::text

#endif // MINSK_ANALYSIS_TEXT_SPAN_HPP
