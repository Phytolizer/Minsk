#include "minsk/analysis/syntax/tree.hpp"
#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/lexer.hpp"
#include "minsk/analysis/syntax/parser.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include <algorithm>
#include <iterator>
#include <utility>

minsk::analysis::syntax::syntax_tree::syntax_tree(
    std::unique_ptr<expression_syntax> root,
    minsk::analysis::syntax::syntax_token &&end_of_file_token,
    minsk::analysis::diagnostic_bag &&diagnostics)
    : m_root(std::move(root)),
      m_end_of_file_token(std::move(end_of_file_token)),
      m_diagnostics(std::move(diagnostics)) {}
const minsk::analysis::syntax::expression_syntax *
minsk::analysis::syntax::syntax_tree::root() const {
  return m_root.get();
}
const minsk::analysis::syntax::syntax_token &
minsk::analysis::syntax::syntax_tree::end_of_file_token() const {
  return m_end_of_file_token;
}
const minsk::analysis::diagnostic_bag &
minsk::analysis::syntax::syntax_tree::diagnostics() const {
  return m_diagnostics;
}
minsk::analysis::syntax::syntax_tree
minsk::analysis::syntax::syntax_tree::parse(std::string_view text) {
  minsk::analysis::syntax::parser parser{text};
  return parser.parse();
}

std::vector<minsk::analysis::syntax::syntax_token>
minsk::analysis::syntax::syntax_tree::parse_tokens(std::string_view text) {
  auto tokens = std::vector<syntax_token>{};
  auto lexer = minsk::analysis::syntax::lexer{text};
  std::copy_if(lexer.begin(), lexer.end(), std::back_inserter(tokens),
               [](const auto &token) {
                 return token.kind() != syntax_kind::end_of_file_token;
               });
  return tokens;
}
