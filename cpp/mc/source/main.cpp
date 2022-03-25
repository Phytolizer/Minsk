#include "minsk/analysis/compilation.hpp"
#include "minsk/analysis/diagnostic_bag.hpp"
#include "minsk/analysis/evaluation_result.hpp"
#include "minsk/analysis/syntax/tree.hpp"
#include "minsk/analysis/text/span.hpp"
#include "minsk/analysis/variable_map.hpp"
#include "minsk/runtime/object.hpp"
#include "rang.hpp"
#include "util/terminal.hpp"
#include <iostream>
#include <memory>
#include <sstream>
#include <string>

int main() {
  std::string input_line;
  bool show_tree = true;
  minsk::analysis::variable_map variables;
  std::ostringstream text_builder;
  std::unique_ptr<minsk::analysis::compilation> previous;

  while (true) {
    std::cout << rang::fg::green << (text_builder.str().empty() ? "» " : "· ")
              << rang::fg::reset << std::flush;
    if (!std::getline(std::cin, input_line)) {
      break;
    }

    if (text_builder.str().empty()) {
      if (input_line == "#showTree") {
        show_tree = !show_tree;
        if (show_tree) {
          std::cout << "Showing parse trees.\n";
        } else {
          std::cout << "Not showing parse trees.\n";
        };
        continue;
      } else if (input_line == "#cls") {
        util::terminal::clear();
        continue;
      } else if (input_line == "#reset") {
        previous = nullptr;
        continue;
      }
    }

    text_builder << input_line << '\n';
    auto syntax_tree =
        minsk::analysis::syntax::syntax_tree::parse(text_builder.str());
    if (!input_line.empty() && !syntax_tree.diagnostics().empty()) {
      continue;
    }
    if (show_tree) {
      syntax_tree.root()->pretty_print();
    }
    auto compilation = std::make_unique<minsk::analysis::compilation>(
        std::move(previous), std::move(syntax_tree));
    minsk::analysis::evaluation_result result =
        compilation->evaluate(&variables);
    if (result.diagnostics().size() > 0) {
      for (const auto &diagnostic : result.diagnostics()) {
        auto line_index = compilation->syntax().text().get_line_index(
            diagnostic.span().start());
        const auto &line = compilation->syntax().text().lines()[line_index];
        auto line_number = line_index + 1;
        auto character = diagnostic.span().start() - line.start() + 1;
        auto prefix_span = minsk::analysis::text::text_span::from_bounds(
            line.start(), diagnostic.span().start());
        auto error_span = diagnostic.span();
        auto suffix_span = minsk::analysis::text::text_span::from_bounds(
            diagnostic.span().end(), line.end());

        auto prefix = compilation->syntax().text().to_string(prefix_span);
        auto error = compilation->syntax().text().to_string(error_span);
        auto suffix = compilation->syntax().text().to_string(suffix_span);
        std::cout << rang::fg::red << "(" << line_number << ", " << character
                  << "): " << diagnostic << '\n'
                  << "    " << rang::fg::reset << prefix << rang::fg::red
                  << error << rang::fg::reset << suffix << '\n';
      }
    } else {
      previous = std::move(compilation);
      std::cout << rang::fg::magenta;
      result.value()->print(std::cout);
      std::cout << rang::fg::reset << '\n';
    }
    text_builder.str("");
  }
  return 0;
}
