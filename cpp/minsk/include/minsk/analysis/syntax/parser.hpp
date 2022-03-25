#ifndef MINSK_PARSER_HPP
#define MINSK_PARSER_HPP

#include "minsk/analysis/diagnostic_bag.hpp"
#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/nodes/statement.hpp"
#include "minsk/analysis/syntax/nodes/unit.hpp"
#include "minsk/analysis/text/source.hpp"
#include "token.hpp"
#include "tree.hpp"
#include <memory>
#include <vector>

namespace minsk::analysis::syntax {

class parser final {
  const text::source_text *m_text;
  std::vector<syntax_token> m_tokens;
  int m_position;
  diagnostic_bag m_diagnostics;

  const syntax_token &peek(int offset) const;
  const syntax_token &current() const;
  syntax_token next_token();
  syntax_token match_token(syntax_kind kind);
  [[nodiscard]] std::unique_ptr<statement_syntax> parse_statement();
  [[nodiscard]] std::unique_ptr<statement_syntax> parse_block_statement();
  [[nodiscard]] std::unique_ptr<statement_syntax> parse_expression_statement();
  [[nodiscard]] std::unique_ptr<expression_syntax> parse_expression();
  [[nodiscard]] std::unique_ptr<expression_syntax>
  parse_assignment_expression();
  [[nodiscard]] std::unique_ptr<expression_syntax>
  parse_binary_expression(int parent_precedence);
  [[nodiscard]] std::unique_ptr<expression_syntax> parse_primary_expression();
  [[nodiscard]] std::unique_ptr<expression_syntax> parse_number_literal();
  [[nodiscard]] std::unique_ptr<expression_syntax> parse_name_expression();
  [[nodiscard]] std::unique_ptr<expression_syntax> parse_boolean_literal();
  [[nodiscard]] std::unique_ptr<expression_syntax>
  parse_parenthesized_expression();

public:
  explicit parser(const text::source_text *text);
  std::unique_ptr<compilation_unit_syntax> parse_compilation_unit();
  diagnostic_bag take_diagnostics();
};

} // namespace minsk::analysis::syntax

#endif // MINSK_PARSER_HPP
