#ifndef MINSK_NODE_HPP
#define MINSK_NODE_HPP

#include "kind.hpp"
#include "minsk/analysis/text/span.hpp"
#include <ostream>
#include <string>
#include <vector>

namespace minsk::analysis::syntax {

class syntax_node {
  static std::ostream &pretty_print(const syntax_node *n, std::ostream &writer,
                                    bool is_to_console, std::string indent,
                                    bool is_last);

public:
  [[nodiscard]] virtual syntax_kind kind() const = 0;
  [[nodiscard]] virtual std::vector<const syntax_node *> children() const = 0;
  [[nodiscard]] virtual text::text_span span() const;
  virtual ~syntax_node() = default;

  void pretty_print() const;
  std::ostream &write_to(std::ostream &os) const;
};

} // namespace minsk::analysis::syntax

std::ostream &operator<<(std::ostream &os,
                         const minsk::analysis::syntax::syntax_node &node);

#endif // MINSK_NODE_HPP
