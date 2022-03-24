#ifndef MINSK_NODE_HPP
#define MINSK_NODE_HPP

#include "kind.hpp"
#include <vector>

namespace minsk::analysis::syntax {

class node {
public:
  [[nodiscard]] virtual syntax_kind kind() const = 0;
  [[nodiscard]] virtual std::vector<const node *> children() const = 0;
};

} // namespace minsk::analysis::syntax

#endif // MINSK_NODE_HPP
