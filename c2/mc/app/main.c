#include <bstrlib.h>
#include <linenoise.h>
#include <minsk/code_analysis/syntax/lexer.h>
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
    struct tagbstring line;
    cstr2tbstr(line, raw_line);

    minsk_syntax_lexer_t lexer = minsk_syntax_lexer_new(&line);
    while (true)
    {
      minsk_syntax_token_t tok = minsk_syntax_lexer_lex(&lexer);
      bstring kind_name = minsk_syntax_kind_display_name(tok.kind);
      printf(
        "%.*s '%.*s'",
        blength(kind_name),
        bdata(kind_name),
        blength(tok.text),
        bdata(tok.text)
      );
      if (tok.value.type != MINSK_OBJECT_TYPE_NIL)
      {
        printf(" ");
        minsk_object_show(tok.value, stdout);
      }
      printf("\n");
      bdestroy(kind_name);
      bdestroy(tok.text);
      if (tok.kind == MINSK_SYNTAX_KIND_END_OF_FILE_TOKEN)
      {
        break;
      }
    }

    free(raw_line);
  }

  linenoiseHistorySave(HISTORY_PATH);
  linenoiseHistoryFree();
  return 0;
}
