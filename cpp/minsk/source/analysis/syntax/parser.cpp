#include "minsk/analysis/syntax/parser.hpp"
#include "fmt/format.h"
#include "minsk/analysis/syntax/lexer.hpp"
#include "minsk/analysis/syntax/nodes/expressions/binary.hpp"
#include "minsk/analysis/syntax/nodes/expressions/literal.hpp"
#include <algorithm>

minsk::analysis::syntax::parser::parser(std::string_view text) : m_position(0) {
  lexer lex{text};
  std::copy_if(lex.begin(), lex.end(), std::back_inserter(m_tokens),
               [](const syntax_token &token) {
                 return token.kind() != syntax_kind::bad_token &&
                        token.kind() != syntax_kind::whitespace_token;
               });
  std::copy(lex.diagnostics().begin(), lex.diagnostics().end(),
            std::back_inserter(m_diagnostics));
}
const minsk::analysis::syntax::syntax_token &
minsk::analysis::syntax::parser::peek(int offset) const {
  int index = m_position + offset;
  if (index >= m_tokens.size()) {
    // return end of file token
    return m_tokens[m_tokens.size() - 1];
  } else {
    return m_tokens[index];
  }
}
const minsk::analysis::syntax::syntax_token &
minsk::analysis::syntax::parser::current() const {
  return peek(0);
}
minsk::analysis::syntax::syntax_token
minsk::analysis::syntax::parser::next_token() {
  syntax_token curr = current();
  m_position += 1;
  return curr;
}
std::unique_ptr<minsk::analysis::syntax::expression_syntax>
minsk::analysis::syntax::parser::parse_expression() {
  return parse_binary_expression(0);
}
minsk::analysis::syntax::syntax_token
minsk::analysis::syntax::parser::match_token(
    minsk::analysis::syntax::syntax_kind kind) {
  if (current().kind() == kind) {
    return next_token();
  }

  m_diagnostics.emplace_back(fmt::format(
      "Expected next token to be <{}>, got <{}> instead",
      magic_enum::enum_name(kind), magic_enum::enum_name(current().kind())));
  return syntax_token{kind, current().position(), "", nullptr};
}
std::unique_ptr<minsk::analysis::syntax::expression_syntax>
minsk::analysis::syntax::parser::parse_binary_expression(
    int parent_precedence) {
  std::unique_ptr<expression_syntax> left = parse_primary_expression();

  while (current().kind() == syntax_kind::plus_token ||
         current().kind() == syntax_kind::minus_token) {
    syntax_token operator_token = next_token();
    std::unique_ptr<expression_syntax> right = parse_primary_expression();
    left = std::make_unique<binary_expression_syntax>(
        std::move(left), std::move(operator_token), std::move(right));
  }

  return left;
}
std::unique_ptr<minsk::analysis::syntax::expression_syntax>
minsk::analysis::syntax::parser::parse_primary_expression() {
  syntax_token number_token = match_token(syntax_kind::number_token);
  return std::make_unique<literal_expression_syntax>(std::move(number_token));
}
const minsk::analysis::diagnostic_bag &
minsk::analysis::syntax::parser::diagnostics() const {
  return m_diagnostics;
}
