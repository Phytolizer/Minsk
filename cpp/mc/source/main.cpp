#include "minsk/analysis/binding/binder.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/compilation.hpp"
#include "minsk/analysis/diagnostic_bag.hpp"
#include "minsk/analysis/evaluation_result.hpp"
#include "minsk/analysis/evaluator.hpp"
#include "minsk/analysis/syntax/tree.hpp"
#include "minsk/runtime/object.hpp"
#include "rang.hpp"
#include "util/terminal.hpp"
#include <algorithm>
#include <iostream>
#include <iterator>
#include <memory>
#include <string>

int main() {
  std::string line;
  bool show_tree = false;
  while (true) {
    std::cout << "> " << std::flush;
    if (!std::getline(std::cin, line)) {
      break;
    }

    if (line == "#showTree") {
      show_tree = !show_tree;
      if (show_tree) {
        std::cout << "Showing parse trees.\n";
      } else {
        std::cout << "Not showing parse trees.\n";
      };
      continue;
    } else if (line == "#cls") {
      util::terminal::clear();
      continue;
    }

    auto syntax_tree = minsk::analysis::syntax::syntax_tree::parse(line);
    if (show_tree) {
      syntax_tree.root()->pretty_print();
    }
    minsk::analysis::compilation compilation{std::move(syntax_tree)};
    minsk::analysis::evaluation_result result = compilation.evaluate();
    if (result.diagnostics().size() > 0) {
      for (const auto &diagnostic : result.diagnostics()) {
        std::cout << rang::fg::red;
        auto prefix = std::string_view{
            line.begin(),
            line.begin() + diagnostic.span().start(),
        };
        auto error = std::string_view{
            line.begin() + diagnostic.span().start(),
            line.begin() + diagnostic.span().end(),
        };
        auto suffix = std::string_view{
            line.begin() + diagnostic.span().end(),
            line.end(),
        };
        std::cout << diagnostic << '\n'
                  << "    " << rang::fg::reset << prefix << rang::fg::red
                  << error << rang::fg::reset << suffix << '\n';
      }
    } else {
      result.value()->print(std::cout);
      std::cout << '\n';
    }
  }
  return 0;
}
