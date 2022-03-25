#ifndef MINSK_ANALYSIS_TEXT_SOURCE_HPP
#define MINSK_ANALYSIS_TEXT_SOURCE_HPP

#include "line.hpp"
#include "minsk/analysis/text/span.hpp"
#include <ostream>
#include <string>
#include <string_view>
#include <vector>

namespace minsk::analysis::text {

class source_text final {
  std::string m_text;
  std::vector<text_line> m_lines;
  explicit source_text(std::string &&text);

  static std::vector<text_line> parse_lines(source_text *st,
                                            std::string_view text);
  static int get_line_break_width(std::string_view text, int i);
  static void add_line(std::vector<text_line> *result, int line_start,
                       int position, int line_break_width);

public:
  static source_text from(std::string &&text);

  int get_line_index(int position) const;

  std::string_view text() const;
  const std::vector<text_line> &lines() const;

  std::string to_string(int start, int length) const;
  std::string to_string(text_span span) const;
  char operator[](int index) const;
  int length() const;
};

} // namespace minsk::analysis::text

std::ostream &operator<<(std::ostream &os,
                         const minsk::analysis::text::source_text &st);

#endif // MINSK_ANALYSIS_TEXT_SOURCE_HPP
