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
#include <textus_coloris.h>

#include "mc/cwd.h"

#define HISTORY_PATH ".minsk-history"

static const struct tc_coloris colors[] = {
  {"RED", "\e[31m"},
  {"GREEN", "\e[32m"},
  {"YELLOW", "\e[33m"},
  {"BLUE", "\e[34m"},
  {"MAGENTA", "\e[35m"},
  {"CYAN", "\e[36m"},
  {"WHITE", "\e[37m"},
  {"BRIGHT_RED", "\e[91m"},
  {"BRIGHT_GREEN", "\e[92m"},
  {"BRIGHT_YELLOW", "\e[93m"},
  {"BRIGHT_BLUE", "\e[94m"},
  {"BRIGHT_MAGENTA", "\e[95m"},
  {"BRIGHT_CYAN", "\e[96m"},
  {"BRIGHT_WHITE", "\e[97m"},
  {"RESET", "\e[0m"},
  {"BOLD", "\e[1m"},
  {"DIM", "\e[2m"},
  {0},
};

extern int
main(int argc, char ** argv)
{
  (void)argc;
  (void)argv;
  if (!mc_set_cwd_from_meson())
  {
    return 1;
  }

  tc_set_colors(colors, TC_COLORIS_MODE_AUTO);

  linenoiseHistoryLoad(HISTORY_PATH);

  bool show_tree = false;
  Arena var_arena = {0};
  minsk_variable_map_t variables = minsk_variable_map_new(&var_arena);

  while (true)
  {
    char * prompt = tc_cstring("#GREEN#»#RESET# ");
    char * raw_line = linenoise(prompt);
    free(prompt);
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
      for (size_t i = 0; i < result.diagnostics.len; i++)
      {
        minsk_diagnostic_t diagnostic = result.diagnostics.ptr[i];

        printf("\n");
        tc_print(
          stdout,
          "#RED#" STRING_FMT "#RESET#\n",
          STRING_ARG(diagnostic.message)
        );
        string_t prefix = STRING_SUB(line, 0, diagnostic.span.start);
        string_t error =
          STRING_SUB_LEN(line, diagnostic.span.start, diagnostic.span.length);
        string_t suffix =
          STRING_SUB_AFTER(line, minsk_text_span_end(diagnostic.span));

        tc_print(
          stdout,
          "    " STRING_FMT "#RED#" STRING_FMT "#RESET#" STRING_FMT "\n",
          STRING_ARG(prefix),
          STRING_ARG(error),
          STRING_ARG(suffix)
        );
      }
      printf("\n");
    }
    else
    {
      tc_print(stdout, "#MAGENTA#");
      minsk_object_show(result.value, stdout);
      tc_print(stdout, "#RESET#\n");
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
