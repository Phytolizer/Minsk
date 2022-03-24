#ifndef MINSK_NODE_HPP
#define MINSK_NODE_HPP

#include "kind.hpp"
#include <ostream>
#include <vector>

namespace minsk::analysis::syntax {

class node {
  static void pretty_print(const node *n, std::ostream &writer,
                           bool is_to_console, std::string indent,
                           bool is_last);

public:
  [[nodiscard]] virtual syntax_kind kind() const = 0;
  [[nodiscard]] virtual std::vector<const node *> children() const = 0;
  virtual ~node() = default;

  void pretty_print() const;
};

} // namespace minsk::analysis::syntax

#endif // MINSK_NODE_HPP
