#include <arena.h>
#include <linenoise.h>
#include <minsk-string/string.h>
#include <minsk/code_analysis/syntax/kind.h>
#include <minsk/code_analysis/syntax/parser.h>
#include <minsk/code_analysis/syntax/token.h>
#include <minsk/runtime/object.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "mc/cwd.h"

#define HISTORY_PATH ".minsk-history"

extern int main(int argc, char** argv)
{
  (void)argc;
  (void)argv;
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

    minsk_syntax_parser_t parser = minsk_syntax_parser_new(&a, line);
    minsk_syntax_tree_t syntax_tree = minsk_syntax_parser_parse(&parser);
    minsk_syntax_node_pretty_print(syntax_tree.root, stdout);

    if (syntax_tree.diagnostics.len > 0)
    {
      printf("\x1b[0;31m");
      for (size_t i = 0; i < syntax_tree.diagnostics.len; i++)
      {
        printf(STRING_FMT "\n", STRING_ARG(syntax_tree.diagnostics.ptr[i]));
      }
      printf("\x1b[0m");
    }

    arena_free(&a);
    free(raw_line);
  }

  linenoiseHistorySave(HISTORY_PATH);
  linenoiseHistoryFree();
  return 0;
}
