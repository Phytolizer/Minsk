#include "minsk/analysis/text/source.hpp"
#include "minsk/analysis/text/span.hpp"

minsk::analysis::text::source_text::source_text(std::string &&text)
    : m_text(std::move(text)), m_lines(parse_lines(this, m_text)) {}

std::vector<minsk::analysis::text::text_line>
minsk::analysis::text::source_text::parse_lines(source_text *st,
                                                std::string_view text) {
  auto result = std::vector<text_line>{};
  int line_start = 0;
  int position = 0;
  while (position < text.size()) {
    auto line_break_width = get_line_break_width(text, position);
    if (line_break_width > 0) {
      add_line(&result, line_start, position, line_break_width);
      position += line_break_width;
      line_start = position;
    } else {
      position += 1;
    }
  }
  add_line(&result, line_start, position, 0);
  return result;
}

int minsk::analysis::text::source_text::get_line_break_width(
    std::string_view text, int i) {
  char c = text[i];
  char look = i + 1 == text.size() ? '\0' : text[i + 1];
  if (c == '\r' && look == '\n') {
    return 2;
  }
  if (c == '\r' || c == '\n') {
    return 1;
  }
  return 0;
}

void minsk::analysis::text::source_text::add_line(
    std::vector<text_line> *result, int line_start, int position,
    int line_break_width) {
  int length = position - line_start;
  int length_including_line_break = length + line_break_width;
  result->emplace_back(line_start, length, length_including_line_break);
}

minsk::analysis::text::source_text
minsk::analysis::text::source_text::from(std::string &&text) {
  return source_text{std::move(text)};
}

int minsk::analysis::text::source_text::get_line_index(int position) {
  int lower = 0;
  int upper = static_cast<int>(m_lines.size()) - 1;

  while (lower <= upper) {
    int index = lower + (upper - lower) / 2;
    int start = m_lines[index].start();

    if (position == start) {
      return index;
    }

    if (start > position) {
      upper = index - 1;
    } else {
      lower = index + 1;
    }
  }

  return lower - 1;
}

std::string_view minsk::analysis::text::source_text::text() const {
  return m_text;
}

const std::vector<minsk::analysis::text::text_line> &
minsk::analysis::text::source_text::lines() const {
  return m_lines;
}

std::string minsk::analysis::text::source_text::to_string(int start,
                                                          int length) const {
  return m_text.substr(start, length);
}

std::string
minsk::analysis::text::source_text::to_string(text_span span) const {
  return to_string(span.start(), span.length());
}

char minsk::analysis::text::source_text::operator[](int index) const {
  return m_text[index];
}

int minsk::analysis::text::source_text::length() const {
  return static_cast<int>(m_text.length());
}

std::ostream &operator<<(std::ostream &os,
                         const minsk::analysis::text::source_text &st) {
  return os << st.text();
}
