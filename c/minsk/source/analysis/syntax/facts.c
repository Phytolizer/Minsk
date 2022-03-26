#include "minsk/analysis/syntax/facts.h"
int facts_binary_operator_precedence(syntax_kind_t kind) {
  switch (kind) {
  case syntax_kind_star_token:
  case syntax_kind_slash_token:
    return 2;
  case syntax_kind_plus_token:
  case syntax_kind_minus_token:
    return 1;
  default:
    return 0;
  }
}
int facts_unary_operator_precedence(syntax_kind_t kind) {
  switch (kind) {
  case syntax_kind_plus_token:
  case syntax_kind_minus_token:
    return 3;
  default:
    return 0;
  }
}
