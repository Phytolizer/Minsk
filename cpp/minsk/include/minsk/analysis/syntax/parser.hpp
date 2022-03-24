#ifndef MINSK_PARSER_HPP
#define MINSK_PARSER_HPP

#include "minsk/analysis/diagnostic_bag.hpp"
#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "token.hpp"
#include "tree.hpp"
#include <vector>

namespace minsk::analysis::syntax {

class parser final {
  std::vector<syntax_token> m_tokens;
  int m_position;
  diagnostic_bag m_diagnostics;

  const syntax_token &peek(int offset) const;
  const syntax_token &current() const;
  syntax_token next_token();
  syntax_token match_token(syntax_kind kind);
  [[nodiscard]] std::unique_ptr<expression_syntax> parse_expression();
  [[nodiscard]] std::unique_ptr<expression_syntax>
  parse_binary_expression(int parent_precedence);
  [[nodiscard]] std::unique_ptr<expression_syntax> parse_primary_expression();
  [[nodiscard]] std::unique_ptr<expression_syntax> parse_number_literal();
  [[nodiscard]] std::unique_ptr<expression_syntax>
  parse_parenthesized_expression();

public:
  explicit parser(std::string_view text);
  syntax_tree parse();
};

} // namespace minsk::analysis::syntax

#endif // MINSK_PARSER_HPP
