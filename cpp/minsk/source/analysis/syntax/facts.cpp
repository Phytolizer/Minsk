#include "minsk/analysis/syntax/facts.hpp"
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
