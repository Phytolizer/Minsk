#include "minsk/analysis/syntax/tree.hpp"
#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/lexer.hpp"
#include "minsk/analysis/syntax/parser.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include "minsk/analysis/text/source.hpp"
#include <algorithm>
#include <iterator>
#include <utility>

using minsk::analysis::diagnostic_bag;
using minsk::analysis::syntax::compilation_unit_syntax;
using minsk::analysis::syntax::lexer;
using minsk::analysis::syntax::syntax_token;
using minsk::analysis::syntax::syntax_tree;
using minsk::analysis::text::source_text;

syntax_tree::syntax_tree(text::source_text &&text) : m_text(std::move(text)) {
  auto parser = syntax::parser{&m_text};
  auto root = parser.parse_compilation_unit();
  auto diagnostics = parser.take_diagnostics();

  m_root = std::move(root);
  m_diagnostics = std::move(diagnostics);
}

const source_text &syntax_tree::text() const { return m_text; }

const compilation_unit_syntax *syntax_tree::root() const {
  return m_root.get();
}

const diagnostic_bag &syntax_tree::diagnostics() const { return m_diagnostics; }

syntax_tree syntax_tree::parse(std::string &&text) {
  auto source_text = text::source_text::from(std::move(text));
  return parse(std::move(source_text));
}

syntax_tree syntax_tree::parse(text::source_text &&text) {
  return syntax_tree{std::move(text)};
}

std::vector<syntax_token> syntax_tree::parse_tokens(std::string &&text) {
  auto tokens = std::vector<syntax_token>{};
  auto source_text = text::source_text::from(std::move(text));
  auto l = lexer{&source_text};
  std::copy_if(l.begin(), l.end(), std::back_inserter(tokens),
               [](const auto &token) {
                 return token.kind() != syntax_kind::end_of_file_token;
               });
  return tokens;
}
