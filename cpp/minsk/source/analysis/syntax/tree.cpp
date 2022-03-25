#include "minsk/analysis/syntax/tree.hpp"
#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/lexer.hpp"
#include "minsk/analysis/syntax/parser.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include "minsk/analysis/text/source.hpp"
#include <algorithm>
#include <iterator>
#include <utility>

minsk::analysis::syntax::syntax_tree::syntax_tree(
    text::source_text &&text, std::unique_ptr<expression_syntax> root,
    minsk::analysis::syntax::syntax_token &&end_of_file_token,
    minsk::analysis::diagnostic_bag &&diagnostics)
    : m_text(std::move(text)), m_root(std::move(root)),
      m_end_of_file_token(std::move(end_of_file_token)),
      m_diagnostics(std::move(diagnostics)) {}
const minsk::analysis::text::source_text &
minsk::analysis::syntax::syntax_tree::text() const {
  return m_text;
}
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
  auto source_text = text::source_text::from(std::string{text});
  return parse(std::move(source_text));
}

minsk::analysis::syntax::syntax_tree
minsk::analysis::syntax::syntax_tree::parse(text::source_text &&text) {
  minsk::analysis::syntax::parser parser{std::move(text)};
  return parser.parse();
}

std::vector<minsk::analysis::syntax::syntax_token>
minsk::analysis::syntax::syntax_tree::parse_tokens(std::string_view text) {
  auto tokens = std::vector<syntax_token>{};
  auto source_text = text::source_text::from(std::string{text});
  auto lexer = minsk::analysis::syntax::lexer{&source_text};
  std::copy_if(lexer.begin(), lexer.end(), std::back_inserter(tokens),
               [](const auto &token) {
                 return token.kind() != syntax_kind::end_of_file_token;
               });
  return tokens;
}
