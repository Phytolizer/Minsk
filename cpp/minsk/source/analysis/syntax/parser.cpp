#include "minsk/analysis/syntax/parser.hpp"
#include "fmt/format.h"
#include "minsk/analysis/syntax/facts.hpp"
#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/lexer.hpp"
#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/nodes/expressions/assignment.hpp"
#include "minsk/analysis/syntax/nodes/expressions/binary.hpp"
#include "minsk/analysis/syntax/nodes/expressions/literal.hpp"
#include "minsk/analysis/syntax/nodes/expressions/name.hpp"
#include "minsk/analysis/syntax/nodes/expressions/parenthesized.hpp"
#include "minsk/analysis/syntax/nodes/expressions/unary.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include "minsk/runtime/object.hpp"
#include <algorithm>
#include <memory>

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
  return parse_assignment_expression();
}
std::unique_ptr<minsk::analysis::syntax::expression_syntax>
minsk::analysis::syntax::parser::parse_assignment_expression() {
  if (peek(0).kind() != syntax_kind::identifier_token ||
      peek(1).kind() != syntax_kind::equals_token) {
    return parse_binary_expression(0);
  }

  auto identifier_token = next_token();
  auto equals_token = next_token();
  auto expression = parse_assignment_expression();
  return std::make_unique<assignment_expression_syntax>(
      std::move(identifier_token), std::move(equals_token),
      std::move(expression));
}
minsk::analysis::syntax::syntax_token
minsk::analysis::syntax::parser::match_token(
    minsk::analysis::syntax::syntax_kind kind) {
  if (current().kind() == kind) {
    return next_token();
  }

  m_diagnostics.report_unexpected_token(current().span(), kind,
                                        current().kind());
  return syntax_token{kind, current().position(), "", nullptr};
}
std::unique_ptr<minsk::analysis::syntax::expression_syntax>
minsk::analysis::syntax::parser::parse_binary_expression(
    int parent_precedence) {
  int unary_operator_precedence =
      facts::unary_operator_precedence(current().kind());
  std::unique_ptr<expression_syntax> left;
  if (unary_operator_precedence != 0 &&
      unary_operator_precedence >= parent_precedence) {
    syntax_token operator_token = next_token();
    std::unique_ptr<expression_syntax> operand =
        parse_binary_expression(unary_operator_precedence);
    left = std::make_unique<unary_expression_syntax>(std::move(operator_token),
                                                     std::move(operand));
  } else {
    left = parse_primary_expression();
  }

  while (true) {
    int precedence = facts::binary_operator_precedence(current().kind());
    if (precedence == 0 || precedence <= parent_precedence) {
      break;
    }
    syntax_token operator_token = next_token();
    std::unique_ptr<expression_syntax> right =
        parse_binary_expression(precedence);
    left = std::make_unique<binary_expression_syntax>(
        std::move(left), std::move(operator_token), std::move(right));
  }

  return left;
}
std::unique_ptr<minsk::analysis::syntax::expression_syntax>
minsk::analysis::syntax::parser::parse_primary_expression() {
  switch (current().kind()) {
  case syntax_kind::open_parenthesis_token:
    return parse_parenthesized_expression();
  case syntax_kind::true_keyword:
  case syntax_kind::false_keyword:
    return parse_boolean_literal();
  case syntax_kind::number_token:
    return parse_number_literal();
  default:
    return parse_name_expression();
  }
}
std::unique_ptr<minsk::analysis::syntax::expression_syntax>
minsk::analysis::syntax::parser::parse_parenthesized_expression() {
  syntax_token open_parenthesis_token =
      match_token(syntax_kind::open_parenthesis_token);
  std::unique_ptr<expression_syntax> expression = parse_expression();
  syntax_token close_parenthesis_token =
      match_token(syntax_kind::close_parenthesis_token);
  return std::make_unique<parenthesized_expression_syntax>(
      std::move(open_parenthesis_token), std::move(expression),
      std::move(close_parenthesis_token));
}
std::unique_ptr<minsk::analysis::syntax::expression_syntax>
minsk::analysis::syntax::parser::parse_number_literal() {
  syntax_token number_token = match_token(syntax_kind::number_token);
  return std::make_unique<literal_expression_syntax>(std::move(number_token));
}
std::unique_ptr<minsk::analysis::syntax::expression_syntax>
minsk::analysis::syntax::parser::parse_name_expression() {
  auto identifier_token = match_token(syntax_kind::identifier_token);
  return std::make_unique<name_expression_syntax>(std::move(identifier_token));
}
std::unique_ptr<minsk::analysis::syntax::expression_syntax>
minsk::analysis::syntax::parser::parse_boolean_literal() {
  bool is_true = current().kind() == syntax_kind::true_keyword;
  syntax_token keyword_token = match_token(
      is_true ? syntax_kind::true_keyword : syntax_kind::false_keyword);
  return std::make_unique<literal_expression_syntax>(
      std::move(keyword_token), std::make_unique<runtime::boolean>(is_true));
}
minsk::analysis::syntax::syntax_tree minsk::analysis::syntax::parser::parse() {
  std::unique_ptr<expression_syntax> expression = parse_expression();
  syntax_token end_of_file_token = match_token(syntax_kind::end_of_file_token);
  return syntax_tree{
      std::move(expression),
      std::move(end_of_file_token),
      std::move(m_diagnostics),
  };
}
