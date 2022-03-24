#ifndef MINSK_TREE_HPP_7DCBF9EE5670440AA18C623FF8CAC78F
#define MINSK_TREE_HPP_7DCBF9EE5670440AA18C623FF8CAC78F

#include "minsk/analysis/diagnostic_bag.hpp"
#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include <memory>
#include <string_view>

namespace minsk::analysis::syntax {

class syntax_tree final {
  std::unique_ptr<expression_syntax> m_root;
  syntax_token m_end_of_file_token;
  diagnostic_bag m_diagnostics;

public:
  syntax_tree(std::unique_ptr<expression_syntax> root,
              syntax_token &&end_of_file_token, diagnostic_bag &&diagnostics);

  const expression_syntax *root() const;
  const syntax_token &end_of_file_token() const;
  const diagnostic_bag &diagnostics() const;

  static syntax_tree parse(std::string_view text);
};

} // namespace minsk::analysis::syntax

#endif // MINSK_TREE_HPP_7DCBF9EE5670440AA18C623FF8CAC78F
