#include "minsk/analysis/syntax/facts.h"
#include "minsk/analysis/syntax/kind.h"
#include <string.h>
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

syntax_kind_t facts_keyword_kind(const char *text) {
  if (strcmp(text, "true") == 0) {
    return syntax_kind_true_keyword;
  }
  if (strcmp(text, "false") == 0) {
    return syntax_kind_false_keyword;
  }
  return syntax_kind_identifier_token;
}
