#include "minsk_test/analysis/text/annotated.hpp"
#include "minsk/analysis/text/span.hpp"
#include <algorithm>
#include <cstddef>
#include <deque>
#include <limits>
#include <ranges>
#include <sstream>
#include <stack>
#include <string>
#include <vector>

using minsk::analysis::text::text_span;

minsk_test::analysis::text::annotated_text::annotated_text(
    std::string &&text, std::vector<text_span> &&spans)
    : m_text(std::move(text)), m_spans(std::move(spans)) {}

std::string
minsk_test::analysis::text::annotated_text::unindent(std::string_view text) {
  auto lines = unindent_lines(text);
  auto ss = std::ostringstream{};
  for (const auto &line : lines) {
    ss << line << '\n';
  }
  return ss.str();
}

minsk_test::analysis::text::annotated_text
minsk_test::analysis::text::annotated_text::parse(std::string_view text) {
  auto unindented_text = unindent(text);
  auto resulting_text = std::ostringstream{};
  auto spans = std::vector<text_span>{};
  auto stack = std::stack<int>{};

  auto position = int{0};

  for (char c : unindented_text) {
    if (c == '[') {
      stack.push(position);
    } else if (c == ']') {
      auto start = stack.top();
      stack.pop();
      auto end = position;
      spans.emplace_back(text_span::from_bounds(start, end));
    } else {
      position += 1;
      resulting_text << c;
    }
  }

  if (!stack.empty()) {
    throw std::runtime_error{"Unbalanced brackets"};
  }

  return annotated_text{resulting_text.str(), std::move(spans)};
}

std::vector<std::string>
minsk_test::analysis::text::annotated_text::unindent_lines(
    std::string_view text) {
  auto lines = std::deque<std::string>{};
  auto line = std::string{};
  auto ss = std::istringstream{std::string{text}};
  while (std::getline(ss, line)) {
    lines.emplace_back(std::move(line));
  }

  auto min_indent = std::string::npos;
  for (const auto &line : lines) {
    auto indent =
        std::ranges::find_if_not(line, [](char c) { return std::isspace(c); });
    if (indent != line.end()) {
      min_indent =
          std::min(min_indent,
                   static_cast<std::string::size_type>(indent - line.begin()));
    }
  }

  if (min_indent != std::string::npos) {
    for (auto &line : lines) {
      line.erase(0, min_indent);
    }
  }

  while (!lines.empty() && std::ranges::find_if_not(lines.front(), [](char c) {
                             return std::isspace(c);
                           }) == lines.front().end()) {
    lines.pop_front();
  }

  while (!lines.empty() && std::ranges::find_if_not(lines.back(), [](char c) {
                             return std::isspace(c);
                           }) == lines.back().end()) {
    lines.pop_back();
  }

  auto result = std::vector<std::string>{};
  result.reserve(lines.size());
  std::ranges::copy(lines, std::back_inserter(result));
  return result;
}

std::string_view minsk_test::analysis::text::annotated_text::text() const {
  return m_text;
}

const std::vector<minsk::analysis::text::text_span> &
minsk_test::analysis::text::annotated_text::spans() const {
  return m_spans;
}
