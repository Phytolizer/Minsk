#include "minsk/analysis/binding/binder.hpp"
#include "minsk/analysis/binding/nodes/expression.hpp"
#include "minsk/analysis/diagnostic_bag.hpp"
#include "minsk/analysis/evaluator.hpp"
#include "minsk/analysis/syntax/tree.hpp"
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
    minsk::analysis::binding::binder binder;
    std::unique_ptr<minsk::analysis::binding::bound_expression> expression;
    if (syntax_tree.diagnostics().size() == 0) {
      expression = binder.bind_expression(syntax_tree.root());
    }
    minsk::analysis::diagnostic_bag diagnostics;
    std::copy(syntax_tree.diagnostics().begin(),
              syntax_tree.diagnostics().end(), std::back_inserter(diagnostics));
    std::copy(binder.diagnostics().begin(), binder.diagnostics().end(),
              std::back_inserter(diagnostics));
    if (diagnostics.size() > 0) {
      std::cout << rang::fg::red;
      for (const auto &diagnostic : diagnostics) {
        std::cout << diagnostic << '\n';
      }
      std::cout << rang::fg::reset;
    } else {
      minsk::analysis::evaluator evaluator{syntax_tree.root()};
      std::cout << evaluator.evaluate() << '\n';
    }
  }
  return 0;
}
