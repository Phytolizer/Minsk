#include "minsk/analysis/syntax/kind.h"
#include <assert.h>
#include <stdbool.h>

void syntax_kind_print(syntax_kind_t kind, FILE* stream) {
  switch (kind) {
#define X(x)                                                                   \
  case syntax_kind_##x:                                                        \
    fprintf(stream, #x);                                                       \
    break;
    SYNTAX_KINDS_X
#undef X
  default:
    assert(false && "corrupt syntax kind");
  }
}

const char* syntax_kind_to_string(syntax_kind_t kind) {
  switch (kind) {
#define X(x)                                                                   \
  case syntax_kind_##x:                                                        \
    return #x;
    SYNTAX_KINDS_X
#undef X
  default:
    assert(false && "corrupt syntax kind");
    return NULL;
  }
}
