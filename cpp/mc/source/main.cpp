#include <iostream>
#include <minsk/analysis/syntax/lexer.hpp>
#include <string>

int main() {
  std::string line;
  while (true) {
    std::cout << "> " << std::flush;
    if (!std::getline(std::cin, line)) {
      break;
    }

    minsk::analysis::syntax::lexer lex{line};
    for (auto tok : lex) {
      std::cout << magic_enum::enum_name(tok.kind()) << '\n';
    }
  }
  return 0;
}
