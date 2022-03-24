#ifndef MINSK_TREE_HPP_7DCBF9EE5670440AA18C623FF8CAC78F
#define MINSK_TREE_HPP_7DCBF9EE5670440AA18C623FF8CAC78F

#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include <memory>

namespace minsk::analysis::syntax {

class syntax_tree {
  std::unique_ptr<expression_syntax> m_root;
  syntax_token m_end_of_file_token;

public:
  syntax_tree(std::unique_ptr<expression_syntax> root,
              syntax_token &&end_of_file_token);

  const expression_syntax *root() const;
  const syntax_token &end_of_file_token() const;
};

} // namespace minsk::analysis::syntax

#endif // MINSK_TREE_HPP_7DCBF9EE5670440AA18C623FF8CAC78F
