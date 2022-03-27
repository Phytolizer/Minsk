#include "minsk/analysis/syntax/parser.hpp"
#include "fmt/format.h"
#include "minsk/analysis/syntax/facts.hpp"
#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/lexer.hpp"
#include "minsk/analysis/syntax/nodes/else.hpp"
#include "minsk/analysis/syntax/nodes/expression.hpp"
#include "minsk/analysis/syntax/nodes/expressions/assignment.hpp"
#include "minsk/analysis/syntax/nodes/expressions/binary.hpp"
#include "minsk/analysis/syntax/nodes/expressions/literal.hpp"
#include "minsk/analysis/syntax/nodes/expressions/name.hpp"
#include "minsk/analysis/syntax/nodes/expressions/parenthesized.hpp"
#include "minsk/analysis/syntax/nodes/expressions/unary.hpp"
#include "minsk/analysis/syntax/nodes/statement.hpp"
#include "minsk/analysis/syntax/nodes/statements/block.hpp"
#include "minsk/analysis/syntax/nodes/statements/expression.hpp"
#include "minsk/analysis/syntax/nodes/statements/if.hpp"
#include "minsk/analysis/syntax/nodes/statements/variable.hpp"
#include "minsk/analysis/syntax/nodes/statements/while.hpp"
#include "minsk/analysis/syntax/nodes/unit.hpp"
#include "minsk/analysis/syntax/token.hpp"
#include "minsk/analysis/text/source.hpp"
#include "minsk/runtime/object.hpp"
#include <algorithm>
#include <memory>

minsk::analysis::syntax::parser::parser(const text::source_text *text)
    : m_text(text), m_position(0) {
  auto lex = lexer{m_text};
  std::copy_if(lex.begin(), lex.end(), std::back_inserter(m_tokens),
               [](const syntax_token &token) {
                 return token.kind() != syntax_kind::bad_token &&
                        token.kind() != syntax_kind::whitespace_token;
               });
  std::copy(lex.diagnostics().begin(), lex.diagnostics().end(),
            std::back_inserter(m_diagnostics));
}
std::unique_ptr<minsk::analysis::syntax::compilation_unit_syntax>
minsk::analysis::syntax::parser::parse_compilation_unit() {
  auto statement = parse_statement();
  auto end_of_file_token = match_token(syntax_kind::end_of_file_token);
  return std::make_unique<compilation_unit_syntax>(
      std::move(statement), std::move(end_of_file_token));
}
minsk::analysis::diagnostic_bag
minsk::analysis::syntax::parser::take_diagnostics() {
  return std::move(m_diagnostics);
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
std::unique_ptr<minsk::analysis::syntax::statement_syntax>
minsk::analysis::syntax::parser::parse_statement() {
  switch (current().kind()) {
  case syntax_kind::open_brace_token:
    return parse_block_statement();
  case syntax_kind::let_keyword:
  case syntax_kind::var_keyword:
    return parse_variable_declaration();
  case syntax_kind::if_keyword:
    return parse_if_statement();
  case syntax_kind::while_keyword:
    return parse_while_statement();
  default:
    return parse_expression_statement();
  }
}
std::unique_ptr<minsk::analysis::syntax::statement_syntax>
minsk::analysis::syntax::parser::parse_block_statement() {
  auto open_brace_token = match_token(syntax_kind::open_brace_token);
  auto statements = std::vector<std::unique_ptr<statement_syntax>>{};
  while (current().kind() != syntax_kind::close_brace_token &&
         current().kind() != syntax_kind::end_of_file_token) {
    auto statement = parse_statement();
    statements.emplace_back(std::move(statement));
  }
  auto close_brace_token = match_token(syntax_kind::close_brace_token);
  return std::make_unique<block_statement_syntax>(std::move(open_brace_token),
                                                  std::move(statements),
                                                  std::move(close_brace_token));
}
std::unique_ptr<minsk::analysis::syntax::statement_syntax>
minsk::analysis::syntax::parser::parse_expression_statement() {
  auto expression = parse_expression();
  return std::make_unique<expression_statement_syntax>(std::move(expression));
}
std::unique_ptr<minsk::analysis::syntax::statement_syntax>
minsk::analysis::syntax::parser::parse_if_statement() {
  auto if_keyword = match_token(syntax_kind::if_keyword);
  auto condition = parse_expression();
  auto then_statement = parse_statement();
  auto else_clause = std::unique_ptr<else_clause_syntax>{nullptr};
  if (current().kind() == syntax_kind::else_keyword) {
    auto else_keyword = match_token(syntax_kind::else_keyword);
    auto else_statement = parse_statement();
    else_clause = std::make_unique<else_clause_syntax>(
        std::move(else_keyword), std::move(else_statement));
  }
  return std::make_unique<if_statement_syntax>(
      std::move(if_keyword), std::move(condition), std::move(then_statement),
      std::move(else_clause));
}
std::unique_ptr<minsk::analysis::syntax::statement_syntax>
minsk::analysis::syntax::parser::parse_while_statement() {
  auto while_keyword = match_token(syntax_kind::while_keyword);
  auto condition = parse_expression();
  auto body = parse_statement();
  return std::make_unique<while_statement_syntax>(
      std::move(while_keyword), std::move(condition), std::move(body));
}
std::unique_ptr<minsk::analysis::syntax::statement_syntax>
minsk::analysis::syntax::parser::parse_variable_declaration() {
  auto is_read_only = current().kind() == syntax_kind::let_keyword;
  auto keyword_token = match_token(is_read_only ? syntax_kind::let_keyword
                                                : syntax_kind::var_keyword);
  auto identifier_token = match_token(syntax_kind::identifier_token);
  auto equals_token = match_token(syntax_kind::equals_token);
  auto initializer = parse_expression();
  return std::make_unique<variable_declaration_syntax>(
      std::move(keyword_token), std::move(identifier_token),
      std::move(equals_token), std::move(initializer));
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
