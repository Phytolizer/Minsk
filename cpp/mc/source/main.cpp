#include "minsk/analysis/evaluator.hpp"
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
    minsk::analysis::syntax::syntax_tree syntax_tree = parser.parse();
    syntax_tree.root()->pretty_print();
    if (syntax_tree.diagnostics().size() > 0) {
      std::cout << rang::fg::red << rang::style::dim;
      for (const auto &diagnostic : syntax_tree.diagnostics()) {
        std::cout << diagnostic << '\n';
      }
      std::cout << rang::fg::reset << rang::style::reset;
    } else {
      minsk::analysis::evaluator evaluator{syntax_tree.root()};
      std::cout << evaluator.evaluate() << '\n';
    }
  }
  return 0;
}