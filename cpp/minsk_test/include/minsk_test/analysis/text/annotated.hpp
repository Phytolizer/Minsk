#ifndef MINSK_TEST_ANALYSIS_TEXT_ANNOTATED_HPP
#define MINSK_TEST_ANALYSIS_TEXT_ANNOTATED_HPP

#include "minsk/analysis/text/span.hpp"
#include <string>
#include <string_view>
#include <vector>
namespace minsk_test::analysis::text {

class annotated_text {
  std::string m_text;
  std::vector<minsk::analysis::text::text_span> m_spans;

  annotated_text(std::string &&text,
                 std::vector<minsk::analysis::text::text_span> &&spans);

  static std::string unindent(std::string_view text);

public:
  static annotated_text parse(std::string_view text);

  static std::vector<std::string> unindent_lines(std::string_view text);

  std::string_view text() const;
  const std::vector<minsk::analysis::text::text_span> &spans() const;
};

} // namespace minsk_test::analysis::text

#endif // MINSK_TEST_ANALYSIS_TEXT_ANNOTATED_HPP
