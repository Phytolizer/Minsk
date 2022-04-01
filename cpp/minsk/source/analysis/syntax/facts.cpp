#include "minsk/analysis/syntax/facts.hpp"
#include "minsk/analysis/syntax/kind.hpp"
#include "minsk/analysis/syntax/nodes/else.hpp"
int minsk::analysis::syntax::facts::binary_operator_precedence(
    minsk::analysis::syntax::syntax_kind kind) {
  switch (kind) {
  case syntax_kind::star_token:
  case syntax_kind::slash_token:
    return 5;
  case syntax_kind::plus_token:
  case syntax_kind::minus_token:
    return 4;
  case syntax_kind::equals_equals_token:
  case syntax_kind::bang_equals_token:
  case syntax_kind::less_token:
  case syntax_kind::less_equals_token:
  case syntax_kind::greater_token:
  case syntax_kind::greater_equals_token:
    return 3;
  case syntax_kind::ampersand_ampersand_token:
  case syntax_kind::ampersand_token:
    return 2;
  case syntax_kind::pipe_pipe_token:
  case syntax_kind::pipe_token:
  case syntax_kind::hat_token:
    return 1;
  default:
    return 0;
  }
}
int minsk::analysis::syntax::facts::unary_operator_precedence(
    minsk::analysis::syntax::syntax_kind kind) {
  switch (kind) {
  case syntax_kind::plus_token:
  case syntax_kind::minus_token:
  case syntax_kind::bang_token:
  case syntax_kind::tilde_token:
    return 6;
  default:
    return 0;
  }
}

minsk::analysis::syntax::syntax_kind
minsk::analysis::syntax::facts::keyword_kind(std::string_view text) {
  if (text == "true") {
    return syntax_kind::true_keyword;
  }
  if (text == "false") {
    return syntax_kind::false_keyword;
  }
  if (text == "let") {
    return syntax_kind::let_keyword;
  }
  if (text == "var") {
    return syntax_kind::var_keyword;
  }
  if (text == "if") {
    return syntax_kind::if_keyword;
  }
  if (text == "else") {
    return syntax_kind::else_keyword;
  }
  if (text == "while") {
    return syntax_kind::while_keyword;
  }
  if (text == "for") {
    return syntax_kind::for_keyword;
  }
  if (text == "to") {
    return syntax_kind::to_keyword;
  }
  return syntax_kind::identifier_token;
}

std::optional<std::string>
minsk::analysis::syntax::facts::get_text(syntax_kind kind) {
  switch (kind) {
  case syntax_kind::plus_token:
    return "+";
  case syntax_kind::minus_token:
    return "-";
  case syntax_kind::star_token:
    return "*";
  case syntax_kind::slash_token:
    return "/";
  case syntax_kind::bang_token:
    return "!";
  case syntax_kind::tilde_token:
    return "~";
  case syntax_kind::ampersand_ampersand_token:
    return "&&";
  case syntax_kind::pipe_pipe_token:
    return "||";
  case syntax_kind::ampersand_token:
    return "&";
  case syntax_kind::pipe_token:
    return "|";
  case syntax_kind::hat_token:
    return "^";
  case syntax_kind::equals_equals_token:
    return "==";
  case syntax_kind::bang_equals_token:
    return "!=";
  case syntax_kind::less_token:
    return "<";
  case syntax_kind::less_equals_token:
    return "<=";
  case syntax_kind::greater_token:
    return ">";
  case syntax_kind::greater_equals_token:
    return ">=";
  case syntax_kind::equals_token:
    return "=";
  case syntax_kind::open_parenthesis_token:
    return "(";
  case syntax_kind::close_parenthesis_token:
    return ")";
  case syntax_kind::open_brace_token:
    return "{";
  case syntax_kind::close_brace_token:
    return "}";
  case syntax_kind::true_keyword:
    return "true";
  case syntax_kind::false_keyword:
    return "false";
  case syntax_kind::let_keyword:
    return "let";
  case syntax_kind::var_keyword:
    return "var";
  case syntax_kind::if_keyword:
    return "if";
  case syntax_kind::else_keyword:
    return "else";
  case syntax_kind::while_keyword:
    return "while";
  case syntax_kind::for_keyword:
    return "for";
  case syntax_kind::to_keyword:
    return "to";
  default:
    return {};
  }
}
