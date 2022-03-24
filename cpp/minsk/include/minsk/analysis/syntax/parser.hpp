#ifndef MINSK_PARSER_HPP
#define MINSK_PARSER_HPP

#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "token.hpp"
#include <vector>

namespace minsk::analysis::syntax {

class parser {
  std::vector<syntax_token> m_tokens;
  int m_position;

  const syntax_token &peek(int offset) const;
  const syntax_token &current() const;
  syntax_token next_token();
  syntax_token match_token(syntax_kind kind);
  std::unique_ptr<expression_syntax>
  parse_binary_expression(int parent_precedence);
  std::unique_ptr<expression_syntax> parse_primary_expression();

public:
  explicit parser(std::string_view text);
  [[nodiscard]] std::unique_ptr<expression_syntax> parse_expression();
};

} // namespace minsk::analysis::syntax

#endif // MINSK_PARSER_HPP
