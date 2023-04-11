#include <arena.h>
#include <linenoise.h>
#include <minsk-string/string.h>
#include <minsk/code_analysis/syntax/kind.h>
#include <minsk/code_analysis/syntax/lexer.h>
#include <minsk/code_analysis/syntax/token.h>
#include <minsk/runtime/object.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "mc/cwd.h"

#define HISTORY_PATH ".minsk-history"

extern int main(int argc, char** argv)
{
  if (!mc_set_cwd_from_meson())
  {
    return 1;
  }

  linenoiseHistoryLoad(HISTORY_PATH);

  while (true)
  {
    const char* prompt = "\x1b[32m»\x1b[0m ";
    char* raw_line = linenoise(prompt);
    if (raw_line == NULL)
    {
      break;
    }
    linenoiseHistoryAdd(raw_line);
    string_t line = STRING_REF_FROM_C(raw_line);
    Arena a = {0};

    minsk_syntax_lexer_t lexer = minsk_syntax_lexer_new(line);
    while (true)
    {
      minsk_syntax_token_t tok = minsk_syntax_lexer_lex(&lexer);
      if (tok.kind == MINSK_SYNTAX_KIND_END_OF_FILE_TOKEN)
      {
        string_free(tok.text);
        break;
      }
      string_t kind_name = minsk_syntax_kind_display_name(&a, tok.kind);
      printf(
        STRING_FMT ": '" STRING_FMT "'",
        STRING_ARG(kind_name),
        STRING_ARG(tok.text)
      );
      string_free_arena(&a, kind_name);
      string_free(tok.text);
      if (tok.value.type != MINSK_OBJECT_TYPE_NIL)
      {
        printf(" ");
        minsk_object_show(tok.value, stdout);
      }
      printf("\n");
    }

    arena_free(&a);
    free(raw_line);
  }

  linenoiseHistorySave(HISTORY_PATH);
  linenoiseHistoryFree();
  return 0;
}
