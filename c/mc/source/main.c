#include "minsk/analysis/syntax/kind.h"
#include "minsk/analysis/syntax/lexer.h"
#include "minsk/analysis/syntax/token.h"
#include "sds.h"
#include "util/line.h"
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(void) {
  char *line = NULL;
  size_t line_len = 0;
  while (true) {
    printf("> ");
    if (!util_read_line(&line, &line_len, stdin)) {
      break;
    }

    lexer_t lexer;
    lexer_init(&lexer, sdsnew(line));

    syntax_token_t token;
    while (true) {
      token = lexer_next_token(&lexer);

      token_print(&token, stdout);
      printf("\n");

      if (token.kind == syntax_kind_end_of_file_token) {
        break;
      }
    }
  }
  free(line);
  return 0;
}
