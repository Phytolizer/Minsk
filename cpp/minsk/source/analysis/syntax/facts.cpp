#include "minsk/analysis/syntax/facts.hpp"
#include "minsk/analysis/syntax/kind.hpp"
int minsk::analysis::syntax::facts::binary_operator_precedence(
    minsk::analysis::syntax::syntax_kind kind) {
  switch (kind) {
  case syntax_kind::star_token:
  case syntax_kind::slash_token:
    return 2;
  case syntax_kind::plus_token:
  case syntax_kind::minus_token:
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
    return 3;
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
  return syntax_kind::identifier_token;
}
