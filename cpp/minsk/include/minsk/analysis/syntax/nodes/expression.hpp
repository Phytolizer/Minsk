#ifndef MINSK_EXPRESSION_HPP
#define MINSK_EXPRESSION_HPP

#include "minsk/analysis/syntax/node.hpp"

namespace minsk::analysis::syntax {

class expression_syntax : public syntax_node {
public:
  virtual ~expression_syntax() = default;
};

} // namespace minsk::analysis::syntax

#endif // MINSK_EXPRESSION_HPP
