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
#include <minsk/data_structures/buf.h>
#include <minsk/runtime/object.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <textus_coloris.h>

#include "mc/cwd.h"
#include "minsk/code_analysis/text/source_text.h"

#define HISTORY_PATH ".minsk-history"

typedef BUF_T(string_t) string_buf_t;

static const struct tc_coloris colors[] = {
  {"RED", "\e[0;31m"},
  {"GREEN", "\e[0;32m"},
  {"YELLOW", "\e[0;33m"},
  {"BLUE", "\e[0;34m"},
  {"MAGENTA", "\e[0;35m"},
  {"CYAN", "\e[0;36m"},
  {"WHITE", "\e[0;37m"},
  {"DIM_RED", "\e[2;31m"},
  {"DIM_GREEN", "\e[2;32m"},
  {"DIM_YELLOW", "\e[2;33m"},
  {"DIM_BLUE", "\e[2;34m"},
  {"DIM_MAGENTA", "\e[2;35m"},
  {"DIM_CYAN", "\e[2;36m"},
  {"DIM_WHITE", "\e[2;37m"},
  {"BRIGHT_RED", "\e[0;91m"},
  {"BRIGHT_GREEN", "\e[0;92m"},
  {"BRIGHT_YELLOW", "\e[0;93m"},
  {"BRIGHT_BLUE", "\e[0;94m"},
  {"BRIGHT_MAGENTA", "\e[0;95m"},
  {"BRIGHT_CYAN", "\e[0;96m"},
  {"BRIGHT_WHITE", "\e[0;97m"},
  {"RESET", "\e[0m"},
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

  string_buf_t text_builder = BUF_INIT;

  while (true)
  {
    char * prompt = tc_cstring(
      (text_builder.len == 0) ? "#GREEN#»#RESET# " : "#GREEN#·#RESET# "
    );
    char * raw_line = linenoise(prompt);
    free(prompt);
    if (raw_line != NULL)
    {
      linenoiseHistoryAdd(raw_line);
    }
    string_t input = raw_line ? STRING_REF_FROM_C(raw_line) : EMPTY_STRING;

    if (text_builder.len == 0)
    {
      if (raw_line == NULL)
      {
        break;
      }
      if (STRING_EQUAL(input, STRING_REF("#showTree")))
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
      if (STRING_EQUAL(input, STRING_REF("#cls")))
      {
        free(raw_line);

        printf("\x1b[2J\x1b[H");
        fflush(stdout);
        continue;
      }
    }

    BUF_PUSH(&text_builder, string_dup(input));

    Arena a = {0};

    string_t text = EMPTY_STRING;
    for (size_t i = 0; i < text_builder.len; i++)
    {
      if (i > 0)
      {
        string_push_arena(&a, &text, '\n');
      }
      string_t line = text_builder.ptr[i];
      string_append_arena(&a, &text, line);
    }

    minsk_syntax_tree_t syntax_tree = minsk_syntax_tree_parse(&a, text);

    if (input.length > 0 && syntax_tree.diagnostics.diagnostics.len > 0)
    {
      arena_free(&a);
      free(raw_line);
      continue;
    }

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
        size_t line_index = minsk_text_source_text_get_line_index(
          syntax_tree.text,
          diagnostic.span.start
        );
        minsk_text_line_t line = syntax_tree.text.lines.ptr[line_index];
        size_t line_number = line_index + 1;
        size_t character = diagnostic.span.start - line.start + 1;

        printf("\n");
        tc_print(stdout, "#RED#");
        printf(
          "(%zu, %zu): " STRING_FMT "\n",
          line_number,
          character,
          STRING_ARG(diagnostic.message)
        );
        tc_print(stdout, "#RESET#");
        minsk_text_span_t prefix_span =
          minsk_text_span_from_bounds(line.start, diagnostic.span.start);
        minsk_text_span_t suffix_span = minsk_text_span_from_bounds(
          minsk_text_span_end(diagnostic.span),
          minsk_text_line_end(line)
        );
        string_t prefix =
          minsk_text_source_text_substring_span(syntax_tree.text, prefix_span);
        string_t error = minsk_text_source_text_substring_span(
          syntax_tree.text,
          diagnostic.span
        );
        string_t suffix =
          minsk_text_source_text_substring_span(syntax_tree.text, suffix_span);

        printf("    " STRING_FMT, STRING_ARG(prefix));
        tc_print(stdout, "#RED#");
        printf(STRING_FMT, STRING_ARG(error));
        tc_print(stdout, "#RESET#");
        printf(STRING_FMT "\n", STRING_ARG(suffix));
      }
      printf("\n");
    }
    else
    {
      tc_print(stdout, "#MAGENTA#");
      minsk_object_show(result.value, stdout);
      tc_print(stdout, "#RESET#\n");
    }

    for (size_t i = 0; i < text_builder.len; i++)
    {
      string_free(text_builder.ptr[i]);
    }
    text_builder.len = 0;
    arena_free(&a);
    free(raw_line);
  }

  for (size_t i = 0; i < text_builder.len; i++)
  {
    string_free(text_builder.ptr[i]);
  }
  BUF_FREE(text_builder);
  arena_free(&var_arena);
  minsk_syntax_facts_free_keyword_table();

  linenoiseHistorySave(HISTORY_PATH);
  linenoiseHistoryFree();
  return 0;
}
