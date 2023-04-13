#include <arena.h>
#include <linenoise.h>
#include <minsk-string/string.h>
#include <minsk/code_analysis/binding/binder.h>
#include <minsk/code_analysis/diagnostic_buf.h>
#include <minsk/code_analysis/evaluator.h>
#include <minsk/code_analysis/syntax/ast/node.h>
#include <minsk/code_analysis/syntax/facts.h>
#include <minsk/code_analysis/syntax/tree.h>
#include <minsk/runtime/object.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "mc/cwd.h"

#define HISTORY_PATH ".minsk-history"

extern int main(int argc, char ** argv)
{
  (void)argc;
  (void)argv;
  if (!mc_set_cwd_from_meson())
  {
    return 1;
  }

  linenoiseHistoryLoad(HISTORY_PATH);

  bool show_tree = false;

  while (true)
  {
    const char * prompt = "\x1b[32m»\x1b[0m ";
    char * raw_line = linenoise(prompt);
    if (raw_line == NULL)
    {
      break;
    }
    linenoiseHistoryAdd(raw_line);
    string_t line = STRING_REF_FROM_C(raw_line);

    if (STRING_EQUAL(line, STRING_REF("#showTree")))
    {
      free(raw_line);

      show_tree = !show_tree;
      printf(
        STRING_FMT "\n",
        STRING_ARG(
          show_tree ? STRING_REF("Showing parse trees.")
                    : STRING_REF("Not showing parse trees.")
        )
      );
      continue;
    }
    if (STRING_EQUAL(line, STRING_REF("#cls")))
    {
      free(raw_line);

      printf("\x1b[2J\x1b[H");
      fflush(stdout);
      continue;
    }

    Arena a = {0};

    minsk_syntax_tree_t syntax_tree = minsk_syntax_tree_parse(&a, line);
    if (show_tree)
    {
      minsk_syntax_node_pretty_print(syntax_tree.root, stdout);
    }
    minsk_diagnostic_buf_t diagnostics = syntax_tree.diagnostics;
    minsk_binder_t binder = minsk_binder_new(&a);
    minsk_bound_node_t bound_expression =
      minsk_binder_bind_expression(&binder, syntax_tree.root);

    BUF_APPEND_ARENA(&a, &diagnostics, binder.diagnostics);

    if (diagnostics.len > 0)
    {
      printf("\x1b[0;31m");
      for (size_t i = 0; i < diagnostics.len; i++)
      {
        printf(STRING_FMT "\n", STRING_ARG(diagnostics.ptr[i]));
      }
      printf("\x1b[0m");
    }
    else
    {
      minsk_evaluator_t e = minsk_evaluator_new(bound_expression);
      minsk_object_t result = minsk_evaluator_evaluate(&e);
      printf("\x1b[0;35m");
      minsk_object_show(result, stdout);
      printf("\x1b[0m\n");
    }

    arena_free(&a);
    free(raw_line);
  }

  minsk_syntax_facts_free_keyword_table();

  linenoiseHistorySave(HISTORY_PATH);
  linenoiseHistoryFree();
  return 0;
}
