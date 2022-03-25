#ifndef MINSK_NODE_HPP
#define MINSK_NODE_HPP

#include "kind.hpp"
#include "minsk/analysis/text/span.hpp"
#include <ostream>
#include <vector>
#include <string>

namespace minsk::analysis::syntax {

class syntax_node {
  static void pretty_print(const syntax_node *n, std::ostream &writer,
                           bool is_to_console, std::string indent,
                           bool is_last);

public:
  [[nodiscard]] virtual syntax_kind kind() const = 0;
  [[nodiscard]] virtual std::vector<const syntax_node *> children() const = 0;
  [[nodiscard]] virtual text::text_span span() const;
  virtual ~syntax_node() = default;

  void pretty_print() const;
};

} // namespace minsk::analysis::syntax

#endif // MINSK_NODE_HPP
