#include "minsk/analysis/syntax/lexer.h"
#include "minsk/analysis/syntax/kind.h"
#include "minsk/analysis/syntax/token.h"
#include "minsk/runtime/object.h"
#include "sds.h"
#include <ctype.h>
#include <errno.h>
#include <limits.h>
#include <stdlib.h>

static char peek(lexer_t *lexer, int offset) {
  int index = lexer->position + offset;
  if (index >= sdslen(lexer->text)) {
    return '\0';
  }
  return lexer->text[index];
}

static char current(lexer_t *lexer) { return peek(lexer, 0); }

void lexer_init(lexer_t *lexer, sds text) {
  lexer->text = text;
  lexer->position = 0;
}

syntax_token_t lexer_next_token(lexer_t *lexer) {
  syntax_kind_t kind = syntax_kind_bad_token;
  int start = lexer->position;
  sds text = NULL;
  object_t *value = NULL;

  if (isspace(current(lexer))) {
    while (isspace(current(lexer))) {
      lexer->position += 1;
    }

    kind = syntax_kind_whitespace_token;
  } else if (isdigit(current(lexer))) {
    while (isdigit(current(lexer))) {
      lexer->position += 1;
    }

    text = sdsnewlen(&lexer->text[start], lexer->position - start);
    errno = 0;
    long long_val = strtol(text, NULL, 10);
    if (errno != 0 || long_val < INT_MIN || long_val > INT_MAX) {
    }

    value = integer_new((int)long_val);
    kind = syntax_kind_number_token;
  } else {
    switch (current(lexer)) {
    case '\0':
      kind = syntax_kind_end_of_file_token;
      break;
    case '+':
      kind = syntax_kind_plus_token;
      lexer->position += 1;
      break;
    case '-':
      kind = syntax_kind_minus_token;
      lexer->position += 1;
      break;
    case '*':
      kind = syntax_kind_star_token;
      lexer->position += 1;
      break;
    case '/':
      kind = syntax_kind_slash_token;
      lexer->position += 1;
      break;
    case '(':
      kind = syntax_kind_open_parenthesis_token;
      lexer->position += 1;
      break;
    case ')':
      kind = syntax_kind_close_parenthesis_token;
      lexer->position += 1;
      break;
    default:
      break;
    }
  }

  if (kind == syntax_kind_bad_token) {
    lexer->position += 1;
  }

  if (text == NULL) {
    text = sdsnewlen(&lexer->text[start], lexer->position - start);
  }

  return (syntax_token_t){
      .base = {.kind = kind, .is_token = true},
      .position = start,
      .text = text,
      .value = value,
  };
}
