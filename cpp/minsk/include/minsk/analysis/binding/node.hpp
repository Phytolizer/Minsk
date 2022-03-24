#ifndef MINSK_ANALYSIS_BINDING_NODE_HPP
#define MINSK_ANALYSIS_BINDING_NODE_HPP

#include "minsk/analysis/binding/kind.hpp"

namespace minsk::analysis::binding {

class bound_node {
public:
  virtual bound_node_kind kind() const = 0;
  virtual ~bound_node() = default;
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_NODE_HPP
