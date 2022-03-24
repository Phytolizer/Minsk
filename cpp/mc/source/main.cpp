#include "minsk/analysis/syntax/parser.hpp"
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
  }
  return 0;
}
