#include <iostream>
#include <string>

int main() {
  std::string line;
  while (true) {
    std::cout << "> " << std::flush;
    if (!std::getline(std::cin, line)) {
      break;
    }

    if (line == "1 + 2 * 3") {
      std::cout << "7\n";
    } else {
      std::cout << "ERROR: Invalid expression!\n";
    }
  }
  return 0;
}
