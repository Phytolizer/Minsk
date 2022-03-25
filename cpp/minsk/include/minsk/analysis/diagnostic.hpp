#ifndef MINSK_ANALYSIS_DIAGNOSTIC_HPP
#define MINSK_ANALYSIS_DIAGNOSTIC_HPP

#include "minsk/analysis/text/span.hpp"
#include <ostream>
#include <string>
#include <string_view>
namespace minsk::analysis {

class diagnostic final {
  text::text_span m_span;
  std::string m_message;

public:
  diagnostic(text::text_span span, std::string message);

  const text::text_span &span() const;
  std::string_view message() const;
};

} // namespace minsk::analysis

std::ostream &operator<<(std::ostream &os,
                         const minsk::analysis::diagnostic &diagnostic);

#endif // MINSK_ANALYSIS_DIAGNOSTIC_HPP
