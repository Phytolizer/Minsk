#include "minsk/analysis/syntax/parser.hpp"
#include "rang.hpp"
#include <iostream>
#include <string>

int main() {
  std::string line;
  while (true) {
    std::cout << "> " << std::flush;
    if (!std::getline(std::cin, line)) {
      break;
    }

    minsk::analysis::syntax::parser parser{line};
    std::unique_ptr<minsk::analysis::syntax::expression_syntax> expression =
        parser.parse_expression();
    expression->pretty_print();
    if (parser.diagnostics().size() > 0) {
      std::cout << rang::fg::red << rang::style::dim;
      for (const auto &diagnostic : parser.diagnostics()) {
        std::cout << diagnostic << '\n';
      }
      std::cout << rang::fg::reset << rang::style::reset;
    }
  }
  return 0;
}
