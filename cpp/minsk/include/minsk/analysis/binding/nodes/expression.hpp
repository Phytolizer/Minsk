#ifndef MINSK_ANALYSIS_BINDING_NODES_EXPRESSION_HPP
#define MINSK_ANALYSIS_BINDING_NODES_EXPRESSION_HPP

#include "minsk/analysis/binding/node.hpp"
#include "minsk/runtime/object.hpp"

namespace minsk::analysis::binding {

class bound_expression : public bound_node {
public:
  virtual runtime::object_kind type() const = 0;
  virtual ~bound_expression() = default;
};

} // namespace minsk::analysis::binding

#endif // MINSK_ANALYSIS_BINDING_NODES_EXPRESSION_HPP
