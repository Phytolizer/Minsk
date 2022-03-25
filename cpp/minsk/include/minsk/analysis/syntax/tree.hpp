#ifndef MINSK_TREE_HPP_7DCBF9EE5670440AA18C623FF8CAC78F
#define MINSK_TREE_HPP_7DCBF9EE5670440AA18C623FF8CAC78F

#include "minsk/analysis/diagnostic_bag.hpp"
#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include "minsk/analysis/text/source.hpp"
#include <memory>
#include <string>
#include <string_view>
#include <vector>

namespace minsk::analysis::syntax {

class syntax_tree final {
  text::source_text m_text;
  std::unique_ptr<expression_syntax> m_root;
  syntax_token m_end_of_file_token;
  diagnostic_bag m_diagnostics;

public:
  syntax_tree(text::source_text &&text, std::unique_ptr<expression_syntax> root,
              syntax_token &&end_of_file_token, diagnostic_bag &&diagnostics);

  const text::source_text &text() const;
  const expression_syntax *root() const;
  const syntax_token &end_of_file_token() const;
  const diagnostic_bag &diagnostics() const;

  static syntax_tree parse(std::string &&text);
  static syntax_tree parse(text::source_text &&text);
  static std::vector<syntax_token> parse_tokens(std::string &&text);
};

} // namespace minsk::analysis::syntax

#endif // MINSK_TREE_HPP_7DCBF9EE5670440AA18C623FF8CAC78F
