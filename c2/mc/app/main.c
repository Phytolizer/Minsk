#include <arena.h>
#include <linenoise.h>
#include <minsk-string/string.h>
#include <minsk/code_analysis/compilation.h>
#include <minsk/code_analysis/diagnostic.h>
#include <minsk/code_analysis/diagnostic_bag.h>
#include <minsk/code_analysis/syntax/ast/node.h>
#include <minsk/code_analysis/syntax/facts.h>
#include <minsk/code_analysis/syntax/tree.h>
#include <minsk/code_analysis/text/span.h>
#include <minsk/code_analysis/variable_map.h>
#include <minsk/runtime/object.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

#include "mc/cwd.h"

#define HISTORY_PATH ".minsk-history"

extern int
main(int argc, char ** argv)
{
  (void)argc;
  (void)argv;
  if (!mc_set_cwd_from_meson())
  {
    return 1;
  }

  linenoiseHistoryLoad(HISTORY_PATH);

  bool show_tree = false;
  Arena var_arena = {0};
  minsk_variable_map_t variables = minsk_variable_map_new(&var_arena);

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
    minsk_compilation_t compilation = minsk_compilation_new(&a, syntax_tree);
    minsk_evaluation_result_t result =
      minsk_compilation_evaluate(&compilation, &variables);

    if (!result.success)
    {
      minsk_text_source_text_t text = syntax_tree.text;

      for (size_t i = 0; i < result.diagnostics.len; i++)
      {
        minsk_diagnostic_t diagnostic = result.diagnostics.ptr[i];
        size_t line_index =
          minsk_text_source_text_get_line_index(text, diagnostic.span.start);
        size_t line_number = line_index + 1;
        size_t character =
          diagnostic.span.start - text._lines.ptr[line_index].start + 1;

        printf("\n");
        printf("\x1b[0;31m");
        printf(
          "(%zu, %zu): " STRING_FMT "\n",
          line_number,
          character,
          STRING_ARG(diagnostic.message)
        );
        printf("\x1b[0m");
        string_t prefix = STRING_SUB(line, 0, diagnostic.span.start);
        string_t error =
          STRING_SUB_LEN(line, diagnostic.span.start, diagnostic.span.length);
        string_t suffix =
          STRING_SUB_AFTER(line, minsk_text_span_end(diagnostic.span));

        printf("    " STRING_FMT, STRING_ARG(prefix));
        printf("\x1b[0;31m");
        printf(STRING_FMT, STRING_ARG(error));
        printf("\x1b[0m");
        printf(STRING_FMT "\n", STRING_ARG(suffix));
      }
      printf("\n");
    }
    else
    {
      printf("\x1b[0;35m");
      minsk_object_show(result.value, stdout);
      printf("\x1b[0m\n");
    }

    arena_free(&a);
    free(raw_line);
  }

  arena_free(&var_arena);
  minsk_syntax_facts_free_keyword_table();

  linenoiseHistorySave(HISTORY_PATH);
  linenoiseHistoryFree();
  return 0;
}
