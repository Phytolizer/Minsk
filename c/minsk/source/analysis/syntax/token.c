#include "minsk/analysis/syntax/token.h"
#include <stdio.h>

void token_print(syntax_token_t *token, FILE *stream) {
  syntax_kind_print(token->base.kind, stream);
  fprintf(stream, " '%s'", token->text);
  if (token->value != NULL) {
    fprintf(stream, " ");
    object_print(token->value, stream);
  }
}
void token_free(syntax_token_t *token) {
  sdsfree(token->text);
  object_free(token->value);
}
